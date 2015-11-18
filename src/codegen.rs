use std::io::Write;
use parsetree::*;
use flowgraph;
use flowgraph::*;
use common::*;
use platform::*;

pub struct IntelPlatform;

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
        try!(writeln!(out, "global {}", fun.ld_name));
        try!(writeln!(out, "{}:", fun.ld_name));
        let sig = plat.get_call_details(&fun.signature);
        // we should generate code that depends on fun.signature
        // but we won't yet
        for stmt in fun.body.stmts.iter() {
            match stmt.action {
                NodeAction::Call(NodeInput::Labeled(ref s), ref called_sig) => {
                    let details = plat.get_call_details(called_sig);
                    try!(writeln!(out, "        call {}", s));
                }
                NodeAction::Assign(ref address) => {
                    if stmt.inputs.len() != 1 {
                        return mkcgerr("assignment must have exactly one input")
                    }
                    match &stmt.inputs[0] {
                        &flowgraph::NodeInput::Labeled(ref label) => {
                            // copy [label] to [address]
                            try!(writeln!(out, "        mov eax, [{}]", label));
                            try!(writeln!(out, "        mov [{}], eax", address));
                        }
                        _ => {
                            return mkcgerr("unsupported assignment")
                        }
                    }
                }
                NodeAction::Return => {
                    try!(writeln!(out, "        ret"));
                }
                _ => {
                    return mkcgerr("unsupported instruction")
                }
            }
        }
    }
    Ok(())
}

