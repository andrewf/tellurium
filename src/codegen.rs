use std::io::Write;
use std::iter::repeat;
use std::collections::HashMap;
use num::FromPrimitive;
use num::BigInt;
use parsetree::*;
use flowgraph;
use flowgraph::*;
use common::*;
use platform::*;
use hw;

struct EntryConvention;
struct DefaultConvention;

impl CallingConvention for EntryConvention {
    fn get_hwreqs(&self, sig: &FunSignature) -> Result<hw::Reqs, Error> {
        if sig.argtypes.len() != 0 {
            mkerr("entry point calling convention does not support arguments")
        } else if sig.return_type.is_some() {
            mkerr("entry point calling convention does not support returns")
        } else {
            Ok(hw::Reqs::new())
        }
    }
    fn codegen_postlude(&self, sig: &FunSignature) -> Result<String, Error> {
        Ok("        mov ebx, -1337\n        mov eax, 1\n        int 0x80".into())
    }
}

impl CallingConvention for DefaultConvention {
    fn get_hwreqs(&self, sig: &FunSignature) -> Result<hw::Reqs, Error> {
        // ignore calling conv
        // ignore saving registers
        // ignore types of functions
        if sig.argtypes.len() > 3 {
            return mkerr("only 3 args allowed");
        }
        let mut reqs = hw::Reqs::new();
        // first three args go in eax, ecx, edx
        for range in sig.argtypes
                        .iter()
                        .zip(vec!["eax".to_string(), "ecx".to_string(), "edx".to_string()])
                        .map(|(_t, r)| hw::Loc::Register(r).into()) {
            reqs.push_before(range);
        }
        // return into eax, if at all
        match sig.return_type {
            box Some(_) => {
                reqs.push_after(hw::Loc::from_regname("eax").into());
            }
            _ => {}
        };
        // finished
        Ok(reqs)
    }
    fn codegen_postlude(&self, sig: &FunSignature) -> Result<String, Error> {
        Ok("        ret".into())
    }
}

// empty type to represent Intel. we're just
// going to hang methods off of it.
pub struct IntelPlatform {
    entry_conv: EntryConvention,
    default_conv: DefaultConvention,
}

impl IntelPlatform {
    pub fn new() -> IntelPlatform {
        IntelPlatform {
            entry_conv: EntryConvention,
            default_conv: DefaultConvention,
        }
    }
}

#[derive(Debug)]
struct HwMove {
    src: hw::Loc,
    dst: hw::Loc,
}

#[derive(Debug)]
struct NodeHw {
    // TODO replace with just variables: Vec<hw::Loc>
    inputs: Vec<hw::Loc>, // || with node.inputs
    outputs: Vec<hw::Loc>, // || with node.outputs
    clobbers: Vec<hw::Loc>,
    moves: Vec<HwMove>, // in order to implement the other fields
}

impl NodeHw {
    pub fn new() -> NodeHw {
        NodeHw {
            inputs: Vec::new(),
            outputs: Vec::new(),
            clobbers: Vec::new(),
            moves: Vec::new(),
        }
    }
}


// expresses relation slot<->hwloc
// need a bimap, but whatever
#[derive(Debug)]
struct HwResidence {
    slots: HashMap<usize, hw::Loc>,
}

impl HwResidence {
    pub fn new() -> HwResidence {
        HwResidence { slots: HashMap::new() }
    }
    pub fn get_slot(&self, loc: &hw::Loc) -> Option<usize> {
        self.slots
            .iter()
            .filter(|&(_, h)| *h == *loc)
            .map(|(s, _)| s)
            .nth(0)
            .map(|s| (*s).clone())
    }
    pub fn get_hwloc(&self, slot: usize) -> Option<hw::Loc> {
        self.slots.get(&slot).map(|h| (*h).clone())
    }
    pub fn set(&mut self, slot: usize, loc: hw::Loc) {
        // TODO check loc uniqueness
        self.slots.insert(slot, loc);
    }
    pub fn unset_slot(&mut self, slot: usize) {
        self.slots.remove(&slot);
    }
    pub fn pick_unused_register(&self) -> Option<hw::Loc> {
        let x86regs = [hw::Loc::from_regname("eax"),
                       hw::Loc::from_regname("ebx"),
                       hw::Loc::from_regname("ecx"),
                       hw::Loc::from_regname("edx"),
                       hw::Loc::from_regname("esi"),
                       hw::Loc::from_regname("edi")];
        for r in &x86regs {
            if self.get_slot(r).is_none() {
                return Some(r.clone());
            }
        }
        return None;
    }
}

#[test]
fn test_hwres() {
    let mut hwr = HwResidence::new();
    hwr.set(0, hw::Loc::Register("eax".into()));
    hwr.set(1, hw::Loc::Register("ecx".into()));
    assert_eq!(hwr.get_slot(&hw::Loc::Register("eax".into())), Some(0));
    assert_eq!(hwr.get_slot(&hw::Loc::Register("ecx".into())), Some(1));
    assert_eq!(hwr.pick_unused_register(),
               Some(hw::Loc::Register("ebx".into())));
}

impl Platform for IntelPlatform {
    fn get_basic_types(&self) -> GlobalTypeNamespace {
        let mut result = GlobalTypeNamespace::with_capacity(10);
        result.insert("u32".to_string(), (4, None));
        result.insert("i32".to_string(), (4, None));
        result.insert("u8".to_string(), (1, None));
        result.insert("ptr_t".to_string(), (4, None));
        return result;
    }
    fn get_pointer_type(&self, _: &DataType) -> Result<String, Error> {
        Ok("ptr_t".to_string())
    }
    fn get_calling_convention(&self, name: &Option<String>)
            -> Result<&CallingConvention, Error> {
        if *name == Some("entry".into()) {
            Ok(&self.entry_conv)
        } else if *name == None {
            Ok(&self.default_conv)
        } else {
            mkerr(&format!("unknown calling convention {:?}", name))
        }
    }

    fn codegen(&self, out: &mut Write, prog: CheckedProgram) -> Result<(), CodeGenError> {
        codegen_x86(out, self, prog)
    }
}

fn codegen_x86(out: &mut Write, plat: &Platform, prog: CheckedProgram) -> Result<(), CodeGenError> {
    try!(writeln!(out, "; generated by Tellurium"));
    try!(writeln!(out, "section .data"));
    // global vars
    for v in prog.global_vars.iter() {
        match (&v.datatype, &v.init) {
            // only support int data types for now
            // assume that's what Basic means
            (&DataType::Basic(_), &Expression::Literal(ref val)) => {
                try!(writeln!(out, "{}:", v.ld_name));
                try!(writeln!(out, "        dw {}", val));
            }
            (ref t, ref i) => {
                return mkcgerr(&format!("unsupported var type {:?} {:?}", t, i));
            }
        }
    }
    try!(writeln!(out, "section .text"));
    // externs
    for ex in prog.externs.iter() {
        try!(writeln!(out, "        extern {}", ex));
    }
    // functions
    for fun in prog.function_definitions {
        try!(codegen_function(out, plat, &fun));
    }
    Ok(())
}

fn codegen_function(out: &mut Write,
                    plat: &Platform,
                    fun: &CheckedFunDef)
                    -> Result<(), CodeGenError> {
    // get convention-specific stuff
    let conv = try!(plat.get_calling_convention(&fun.signature.convention));
    // make the label
    try!(writeln!(out, "global {}", fun.ld_name));
    try!(writeln!(out, "{}:", fun.ld_name));
    // ra
    let (framesize, nodehw) = try!(register_allocation(&fun.body));
    assert_eq!(nodehw.len(), fun.body.nodes.len());
    // generate prelude
    try!(writeln!(out, "{}", try!(conv.codegen_prelude(&fun.signature))));
    // move stack
    try!(writeln!(out, "        sub esp, {}", framesize));
    // generate code per statement
    for (stmt, hw) in fun.body.nodes.iter().zip(nodehw) {
        // help
        try!(writeln!(out, "; node {:?}", stmt));
        try!(writeln!(out, "; hw {:?}", hw));
        for tomove in hw.moves {
            try!(generate_move(out, &tomove.src, &tomove.dst));
        }
        match stmt.action {
            NodeAction::Call(ref _called_sig) => {
                let call_loc = try!(hw.inputs
                                      .last()
                                      .ok_or(CodeGenError::Other("no fn to call".into())));
                let call_loc = match call_loc {
                    &hw::Loc::Mem(box ref addr, 0) => {
                        addr  // take address
                    }
                    &hw::Loc::Mem(_, _) => {
                        return mkcgerr("call should not have an offset!");
                    }
                    ref loc => *loc, // else, no change
                };
                try!(writeln!(out, "        call {}", try!(hwloc_ref(call_loc))));
            }
            NodeAction::CopyOnly => {} // all the work is in move generator loop
            NodeAction::Return => {
                // generate postlude
                try!(writeln!(out, "        add esp, {}", framesize));
                try!(writeln!(out, "{}", try!(conv.codegen_postlude(&fun.signature))));
            }
        }
    }
    Ok(())
}

fn register_allocation(graph: &FlowGraph)
                       -> Result<(u64, Vec<NodeHw>), CodeGenError> {
    let numnodes = graph.nodes.len();
    let framesize: u64 = 4 * (graph.localslots.len() as u64);
    assert!(numnodes > 0);
    let mut residence = HwResidence::new();
    let mut hw = Vec::with_capacity(numnodes);
    // initialize residence with the requirements of the calling convention
    // graph.reqs.befores correspond to first n slots.
    for (i, var_id) in graph.reqs.befores.iter().enumerate() {
        if let Some(hwloc) = graph.reqs.variables[*var_id].concrete() {
            residence.set(i, hwloc.clone());
        } else {
            panic!("hwreqs of a FlowGraph must have definite locations");
        }
    }
    // assume all slots live forever after being introduced
    for node in graph.nodes.iter() {
        // where we stick our results
        // need a mutable working space to put our results
        // that still allows hw::Ranges (so can't use NodeHw here)
        assert_eq!(node.inputs.len(), node.hwreqs.befores.len());
        assert_eq!(node.outputs.len(), node.hwreqs.afters.len());
        let mut reqs = node.hwreqs.clone();
        // for each node, we need to keep track of the hwlocs we've
        // already decided to use so we don't trample our own work.
        let mut needed_befores = HwResidence::new();
        let mut needed_afters = HwResidence::new();
        // iterate through concrete ranges and track the fact
        // that these hwreqs have to be satisfied
        for (hwloc, slot) in reqs.befores
                                 .iter()
                                 .zip(&node.inputs)
                                 .filter_map(|(var_id, slot)| {
                                     reqs.variables[*var_id]
                                         .concrete()
                                         .map(|loc| (loc, slot))
                                 }) {
            // range is concrete.
            // fail if anything from this node is using the slot
            if needed_befores.get_slot(hwloc).is_some() {
                return mkcgerr("; conflict in befores of some node");
            }
            // add to needed_befores
            needed_befores.set(*slot, hwloc.clone());
        }
        // iter through generic hwreqs
        // pick values for them.
        for (var_id, slot) in reqs.befores
                                  .iter()
                                  .zip(&node.inputs) {
            let var = &mut reqs.variables[*var_id];
            if var.concrete().is_none() {
                // if generic, choose a location
                // TODO if slot is naturally an imm or label or something, pick that.
                let r = try!(needed_befores.pick_unused_register()
                                           .ok_or(CodeGenError::Other("used all registers for \
                                                                       input".into())));
                needed_befores.set(*slot, r.clone());
                *var = r.into();
            }
        }
        // now do it all again for afters
        // record the definite ones (possibly with contributions from befores in loop above)
        for (var_id, slot) in reqs.afters
                                  .iter()
                                  .zip(&node.outputs)
                                  .filter(|&(var_id, _)| {
                                      reqs.variables[*var_id].concrete().is_some()
                                  }) {
            let hwloc = reqs.variables[*var_id].concrete().unwrap(); // guaranteed by filter
            if needed_afters.get_slot(hwloc).is_some() {
                return mkcgerr("conflict in afters of some node");
            }
            needed_afters.set(*slot, hwloc.clone());
        }
        // pick values for ranged afters
        for (var_id, slot) in reqs.afters
                                  .iter()
                                  .zip(&node.outputs) {
            let var = &mut reqs.variables[*var_id];
            if var.concrete().is_none() {
                let r = try!(needed_afters.pick_unused_register()
                                          .ok_or(CodeGenError::Other("used all registers for \
                                                                      output"
                                                                         .into())));
                needed_befores.set(*slot, r.clone());
                *var = r.into();
            }
        }
        // collect the variables into actual, final lists of hwlocs
        let before_hw: Vec<hw::Loc> = try!(reqs.befores
                                             .iter()
                                             .map(|var_id| {
                                                 reqs.variables[*var_id]
                                                     .concrete()
                                                     .map(|loc| (*loc).clone())
                                                     .ok_or(CodeGenError::Other("failed to alloc \
                                                                                 hw for input"
                                                                                    .into()))
                                             })
                                             .collect());
        let after_hw: Vec<hw::Loc> = try!(reqs.afters
                                            .iter()
                                            .map(|var_id| {
                                                reqs.variables[*var_id]
                                                    .concrete()
                                                    .map(|loc| (*loc).clone())
                                                    .ok_or(CodeGenError::Other("failed to alloc \
                                                                                hw for input"
                                                                                   .into()))
                                            })
                                            .collect());
        let clobber_hw = Vec::new();
        // generate moves
        let mut moves = Vec::new();
        // first evictions
        // in principle there could still be a non-concrete variable if it was never used
        // we ignore that case, since it doesn't hurt anything.
        for hwloc in reqs.variables.iter().filter_map(|r| r.concrete()) {
            if let Some(existing_slot) = residence.get_slot(hwloc) {
                // evict existing_slot from hwloc
                // choose new location
                let new_loc = hw::Loc::stack((existing_slot as i64) * 4);
                // make the change
                residence.unset_slot(existing_slot);
                residence.set(existing_slot, new_loc.clone());
                moves.push(HwMove {
                    src: hwloc.clone(),
                    dst: new_loc.clone(),
                });
            } else {
                // nothing to do
            }
        }
        // move inputs into their correct positions
        for (dest_hwloc, slot) in before_hw.iter().zip(&node.inputs) {
            if let Some(res_loc) = residence.get_hwloc(*slot) {
                // move from res_loc to dest_hwloc
                moves.push(HwMove {
                    src: res_loc.clone(),
                    dst: dest_hwloc.clone(),
                });
            } else {
                return mkcgerr("tried to reference slot before it's defined");
            }
        }
        // change residences to reflect outputs.
        for (hwloc, slot) in after_hw.iter().zip(&node.outputs) {
            residence.set(*slot, hwloc.clone());
        }
        // collect all data
        hw.push(NodeHw {
            inputs: before_hw,
            outputs: after_hw,
            clobbers: clobber_hw,
            moves: moves,
        });
    }
    Ok((framesize, hw))
}

// given a hwloc, generate an assembly operand string that references that location
fn hwloc_ref(loc: &hw::Loc) -> Result<String, CodeGenError> {
    match loc {
        &hw::Loc::Register(ref s) => {
            // intel syntax ftw
            Ok(s.clone())
        }
        &hw::Loc::Imm(ref n) => Ok(n.to_str_radix(10)),
        &hw::Loc::Label(ref s) => Ok(s.clone()),
        &hw::Loc::StackPtr => Ok("esp".into()),  // esp points to tip of stack
        &hw::Loc::Mem(box ref address, offset) => {
            // Ok(s.clone())
            Ok(format!("DWORD [{} + {}]", try!(hwloc_ref(address)), offset))
        }
        // TODO beware double dereferences.
    }
}

fn generate_move(out: &mut Write, src: &hw::Loc, dst: &hw::Loc) -> Result<(), CodeGenError> {
    match (src, dst) {
        (_, &hw::Loc::Imm(_)) => mkcgerr("can't move into an immediate value"),
        // TODO parameterize with clobberable swap register, size of move
        (&hw::Loc::Mem(_, _), &hw::Loc::Mem(_, _)) => mkcgerr("can't move from mem to mem"),
        _ => {
            try!(writeln!(out,
                          "        mov {}, {}",
                          try!(hwloc_ref(dst)),
                          try!(hwloc_ref(src))));
            Ok(())
        }
    }
}

#[test]
fn test_hwloc_ref() {
    assert_eq!(hwloc_ref(&hw::Loc::from_regname("eax")).expect("oops"), "eax");
    assert_eq!(hwloc_ref(&hw::Loc::Imm(FromPrimitive::from_i64(45).unwrap())).expect("oops"),
               "45");
    assert_eq!(hwloc_ref(&hw::Loc::labelled_var("global")).expect("oops"),
               "DWORD [global + 0]");
    assert_eq!(hwloc_ref(&hw::Loc::stack(16)).expect("oops"),
               format!("DWORD [esp + 16]"));
}

#[test]
fn test_movegen() {
    use std::str;
    let mut w = Vec::<u8>::new();
    generate_move(&mut w,
                  &hw::Loc::Imm(FromPrimitive::from_i64(42).unwrap()),
                  &hw::Loc::from_regname("eax"))
        .expect("failed to generate move");
    assert_eq!(str::from_utf8(&w).expect("invalid utf8?"), "        mov eax, 42\n");
}

#[test]
fn test_ra() {
    // build test flowgraph
    // fun testy(arg1 i32, arg2 i32) do
    //     global = 42
    //     somefun(global, 13)
    //     somefun(arg2, arg1) // reverse order!
    // end
    //
    // nodes:
    //   imm 42
    //   copy global
    //   load global
    //   imm 13
    //   load somefun
    //   call
    //   load somefun
    //   call with 1, 0
    //   return
    let t_i32 = DataType::Basic("i32".into());
    let fun2ints_sig = FunSignature {
        argtypes: vec![t_i32.clone(), t_i32.clone()],
        return_type: box None,
        convention: None,
    };
    let fun2ints = DataType::Composite(CompositeType::Fun(fun2ints_sig.clone()));
    // create slots
    let slots = vec![
        t_i32.clone(), // 0 first arg
        t_i32.clone(), // 1 second arg
        t_i32.clone(), // 2 imm 42
        t_i32.clone(), // 3 first use of global
        t_i32.clone(), // 4 imm 13
        fun2ints.clone(), // 5 first use of somefun
        fun2ints.clone(), // 6 second use of somefun
    ];
    // create list of nodes
    let default_node = Node {
        action: NodeAction::CopyOnly,
        inputs: vec![],
        outputs: vec![],
        hwreqs: hw::Reqs::new(),
    };
    let nodes = vec![
        node_from_hw(hw::Loc::Imm(bigint(42)), 2),
        // write global
        Node {
            inputs: vec![2],
            hwreqs: hw::Reqs::with_befores(vec![hw::Loc::labelled_var("global").into()]),
            ..default_node.clone()
        },
        node_from_hw(hw::Loc::labelled_var("global"), 3), // load global
        node_from_hw(hw::Loc::Imm(bigint(13)), 4), // load 13
        node_from_hw(hw::Loc::labelled_var("somefun"), 5),
        Node { // call somefun
            action: NodeAction::Call(fun2ints_sig.clone()),
            inputs: vec![3, 4, 5],
            outputs: vec![],
            hwreqs: hw::Reqs::with_befores(vec![hw::Loc::Register("eax".into()).into(),
                                hw::Loc::Register("ecx".into()).into(),
                                hw::Range::new()]),
        },
        node_from_hw(hw::Loc::labelled_var("somefun"), 6),
        Node { // call somefun again
            action: NodeAction::Call(fun2ints_sig.clone()),
            inputs: vec![1, 0, 6],
            outputs: vec![],
            hwreqs: hw::Reqs::with_befores(vec![hw::Loc::Register("eax".into()).into(),
            hw::Loc::Register("ecx".into()).into(),
            hw::Range::new()]),
        },
        Node {
            action: NodeAction::Return,
            ..default_node
        },
    ];
    // make hwreqs for the graph as a whole
    let mut reqs = hw::Reqs::new();
    reqs.variables.push(hw::Loc::from_regname("eax").into()); // first arg
    reqs.variables.push(hw::Loc::from_regname("ecx").into()); // second arg
    reqs.befores.push(0);
    reqs.befores.push(1);
    let graph = FlowGraph {
        nodes: nodes,
        localslots: slots,
        reqs: reqs,
    };
    // do register allocation
    let (framesize, nodehw) = register_allocation(&graph).expect("failed to RA");
    // check that obvious properties hold
    assert_eq!(framesize, 28);
    assert_eq!(nodehw.len(), graph.nodes.len());  // these must be in parallel
    // check hwalloc of first call (node 5) is ok
    assert_eq!(nodehw[5].inputs.len(), 3);
    assert_eq!(nodehw[5].inputs[0], hw::Loc::Register("eax".into()));
    assert_eq!(nodehw[5].inputs[1], hw::Loc::Register("ecx".into()));
    assert_eq!(nodehw[5].inputs[2], hw::Loc::Register("ebx".into()));  // callee
    // check hwalloc of second call (node 7)
    assert_eq!(nodehw[7].inputs.len(), 3);
    assert_eq!(nodehw[7].inputs[0], hw::Loc::Register("eax".into()));
    assert_eq!(nodehw[7].inputs[1], hw::Loc::Register("ecx".into()));
    assert_eq!(nodehw[7].inputs[2], hw::Loc::Register("ebx".into()));  // callee
}
