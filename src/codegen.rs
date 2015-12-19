use std::io::Write;
use num::FromPrimitive;
use num::BigInt;
use parsetree::*;
use flowgraph;
use flowgraph::*;
use common::*;
use platform::*;

// empty type to represent Intel. we're just
// going to hang methods off of it.
pub struct IntelPlatform;

struct HwMove {
    src: HwLoc,
    dst: HwLoc,
}

struct NodeHw {
    inputs: Vec<HwLoc>,  // || with node.inputs
    outputs: Vec<HwLoc>, // || with node.outputs
    clobbers: Vec<HwLoc>,
    moves: Vec<HwMove>,  // in order to implement the other fields
}

impl Platform for IntelPlatform {
    fn get_basic_types(&self) -> GlobalTypeNamespace {
        let mut result = GlobalTypeNamespace::with_capacity(10);
        result.insert("u32".to_string(), (4, None));
        result.insert("i32".to_string(), (4, None));
        result.insert("u8".to_string(), (1, None));
        result.insert("ptr_t".to_string(), (4, None));
        return result
    }
    fn get_pointer_type(&self, _: &DataType) -> Result<String, Error> {
        Ok("ptr_t".to_string())
    }
    fn get_call_details(&self, sig: &FunSignature) -> CallDetails {
        // ignore calling conv
        // ignore saving registers
        // ignore types of functions
        if sig.argtypes.len() > 3 {
            panic!("only three arguments for now")
        }
        let args = sig.argtypes.iter()
            .zip(vec!["eax".to_string(), "ecx".to_string(), "edx".to_string()])
            .map(|(_t, r)| { HwLoc::Register(r) })
            .collect();
        let rets = match sig.return_type {
            box Some(ref ret) => vec![HwLoc::Register("eax".into())],
            _ => Vec::new()
        };
        CallDetails {
            args: args,
            returns: rets,
            clobbers: Vec::new(),
        }
    }
    fn codegen(&self, out: &mut Write, prog: CheckedProgram)
        -> Result<(), CodeGenError>
    {
        codegen_x86(out, self, prog)
    }
}

fn codegen_x86(out: &mut Write, plat: &Platform, prog: CheckedProgram)
    -> Result<(), CodeGenError>
{
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
                return mkcgerr(&format!(
                    "unsupported var type {:?} {:?}", t, i));
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

fn codegen_function(out: &mut Write, plat: &Platform, fun: &CheckedFunDef)
    -> Result<(), CodeGenError>
{
    try!(writeln!(out, "global {}", fun.ld_name));
    try!(writeln!(out, "{}:", fun.ld_name));
    let sig = plat.get_call_details(&fun.signature);
    // ra
    //let allocations = try!(register_allocation(plat, fun));
    // generate code per statement
    for stmt in fun.body.nodes.iter() {
        match stmt.action {
            //NodeAction::Call(NodeInput::Labeled(ref s), ref called_sig) => {
            //    let details = plat.get_call_details(called_sig);
            //    try!(writeln!(out, "        call {}", s));
            //}
            //NodeAction::Assign(ref address) => {
            //    if stmt.inputs.len() != 1 {
            //        return mkcgerr("assignment must have exactly one input")
            //    }
            //    match &stmt.inputs[0] {
            //        &flowgraph::NodeInput::Labeled(ref label) => {
            //            // copy [label] to [address]
            //            try!(writeln!(out, "        mov eax, [{}]", label));
            //            try!(writeln!(out, "        mov [{}], eax", address));
            //        }
            //        _ => {
            //            return mkcgerr("unsupported assignment")
            //        }
            //    }
            //}
            //NodeAction::Return => {
            //    try!(writeln!(out, "        ret"));
            //}
            _ => {
                return mkcgerr("unsupported instruction")
            }
        }
    }
    Ok(())
}

fn hwloc_ref(loc: &HwLoc) -> Result<String, CodeGenError> {
    match loc {
        &HwLoc::Register(ref s) => {
            // intel syntax ftw
            Ok(s.clone())
        }
        &HwLoc::Label(ref s) => {
            Ok(s.clone())
        }
        &HwLoc::Imm(ref n) => {
            Ok(n.to_str_radix(10))
        }
        &HwLoc::Stack(offset) => {
            Ok(format!("[esp + {}]", offset))
        }
    }
}

fn generate_move(out: &mut Write, src: &HwLoc, dst: &HwLoc) -> Result<(), CodeGenError> {
    match dst {
        &HwLoc::Register(_) => {
            try!(writeln!(out, "mov {}, {}", try!(hwloc_ref(dst)), try!(hwloc_ref(src))));
            Ok(())
        }
        &HwLoc::Label(_) => {
            try!(writeln!(out, "mov {}, {}", try!(hwloc_ref(dst)), try!(hwloc_ref(src))));
            Ok(())
        }
        &HwLoc::Stack(_) => {
            try!(writeln!(out, "mov {}, {}", try!(hwloc_ref(dst)), try!(hwloc_ref(src))));
            Ok(())
        }
        &HwLoc::Imm(_) => {
            return mkcgerr("can't move into an immediate value");
        }
    }
}

fn register_allocation(plat: &Platform, fun: &CheckedFunDef)
    -> Result<(u64, Vec<NodeHw>), CodeGenError>
{
    let framesize = 0;
    let hw = Vec::new();
    Ok((framesize, hw))
}

#[test]
fn test_hwloc_ref() {
    assert_eq!(hwloc_ref(&HwLoc::Label("global".into())).expect("oops"), "global");
    assert_eq!(hwloc_ref(&HwLoc::Register("eax".into())).expect("oops"), "eax");
    assert_eq!(hwloc_ref(&HwLoc::Stack(16)).expect("oops"), format!("[esp + 16]"));
    assert_eq!(hwloc_ref(&HwLoc::Imm(FromPrimitive::from_i64(45).unwrap())).expect("oops"), "45");
}

#[test]
fn test_movegen() {
    use std::str;
    let mut w = Vec::<u8>::new();
    generate_move(&mut w, &HwLoc::Imm(FromPrimitive::from_i64(42).unwrap()),
                          &HwLoc::Register("eax".into()))
        .expect("failed to generate move");
    assert_eq!(str::from_utf8(&w).expect("invalid utf8?"), "mov eax, 42\n");

}

