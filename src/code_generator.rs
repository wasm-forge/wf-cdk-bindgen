use candid::pretty::utils::*;
use candid::types::{Field, Function, Label, SharedLabel, Type, TypeEnv, TypeInner};
use candid_parser::bindings::analysis::{chase_actor, infer_rec};
use convert_case::{Case, Casing};
use pretty::RcDoc;
use std::collections::BTreeSet;

#[derive(Clone)]
pub enum Target {
    CanisterCall,
    Agent,
    CanisterStub,
    Builder,
}

#[derive(Clone)]
pub struct Config {
    candid_crate: String,
    type_attributes: String,
    canister_id: Option<candid::Principal>,
    canister_wasm_path: Option<String>,
    service_name: String,
    target: Target,
}

impl Config {
    pub fn new() -> Self {
        Config {
            candid_crate: "candid".to_string(),
            type_attributes: "".to_string(),
            canister_id: None,
            canister_wasm_path: None,
            service_name: "service".to_string(),
            target: Target::CanisterCall,
        }
    }
    pub fn set_candid_crate(&mut self, name: String) -> &mut Self {
        self.candid_crate = name;
        self
    }
    /// Applies to all types for now
    pub fn set_type_attributes(&mut self, attr: String) -> &mut Self {
        self.type_attributes = attr;
        self
    }
    /// Only generates SERVICE struct if canister_id is not provided
    pub fn set_canister_id(&mut self, id: candid::Principal) -> &mut Self {
        self.canister_id = Some(id);
        self
    }

    pub fn set_canister_wasm_path(&mut self, wasm_path: String) -> &mut Self {
        self.canister_wasm_path = Some(wasm_path);
        self
    }
    /// Service name when canister id is provided
    pub fn set_service_name(&mut self, name: String) -> &mut Self {
        self.service_name = name;
        self
    }
    pub fn set_target(&mut self, name: Target) -> &mut Self {
        self.target = name;
        self
    }
}
impl Default for Config {
    fn default() -> Self {
        Self::new()
    }
}

type RecPoints<'a> = BTreeSet<&'a str>;
// The definition of tuple is language specific.
pub(crate) fn is_tuple(fs: &[Field]) -> bool {
    if fs.is_empty() {
        return false;
    }
    !fs.iter()
        .enumerate()
        .any(|(i, field)| field.id.get_id() != (i as u32))
}
static KEYWORDS: [&str; 51] = [
    "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for",
    "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return",
    "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe", "use", "where",
    "while", "async", "await", "dyn", "abstract", "become", "box", "do", "final", "macro",
    "override", "priv", "typeof", "unsized", "virtual", "yield", "try",
];
fn ident_(id: &str, case: Option<Case>) -> (RcDoc, bool) {
    if id.is_empty()
        || id.starts_with(|c: char| !c.is_ascii_alphabetic() && c != '_')
        || id.chars().any(|c| !c.is_ascii_alphanumeric() && c != '_')
    {
        return (RcDoc::text(format!("_{}_", candid::idl_hash(id))), true);
    }
    let (is_rename, id) = if let Some(case) = case {
        let new_id = id.to_case(case);
        (new_id != id, new_id)
    } else {
        (false, id.to_owned())
    };
    if ["crate", "self", "super", "Self", "Result", "Principal"].contains(&id.as_str()) {
        (RcDoc::text(format!("{id}_")), true)
    } else if KEYWORDS.contains(&id.as_str()) {
        (RcDoc::text(format!("r#{id}")), is_rename)
    } else {
        (RcDoc::text(id), is_rename)
    }
}
fn ident(id: &str, case: Option<Case>) -> RcDoc {
    ident_(id, case).0
}

fn pp_ty<'a>(ty: &'a Type, recs: &RecPoints) -> RcDoc<'a> {
    use TypeInner::*;
    match ty.as_ref() {
        Null => str("()"),
        Bool => str("bool"),
        Nat => str("candid::Nat"),
        Int => str("candid::Int"),
        Nat8 => str("u8"),
        Nat16 => str("u16"),
        Nat32 => str("u32"),
        Nat64 => str("u64"),
        Int8 => str("i8"),
        Int16 => str("i16"),
        Int32 => str("i32"),
        Int64 => str("i64"),
        Float32 => str("f32"),
        Float64 => str("f64"),
        Text => str("String"),
        Reserved => str("candid::Reserved"),
        Empty => str("candid::Empty"),
        Var(ref id) => {
            let name = ident(id, Some(Case::Pascal));
            if recs.contains(id.as_str()) {
                str("Box<").append(name).append(">")
            } else {
                name
            }
        }
        Principal => str("Principal"),
        Opt(ref t) => str("Option").append(enclose("<", pp_ty(t, recs), ">")),
        // It's a bit tricky to use `deserialize_with = "serde_bytes"`. It's not working for `type t = blob`
        Vec(ref t) if matches!(t.as_ref(), Nat8) => str("serde_bytes::ByteBuf"),
        Vec(ref t) => str("Vec").append(enclose("<", pp_ty(t, recs), ">")),
        Record(ref fs) => pp_record_fields(fs, recs, ""),
        Variant(_) => unreachable!(), // not possible after rewriting
        Func(_) => unreachable!(),    // not possible after rewriting
        Service(_) => unreachable!(), // not possible after rewriting
        Class(_, _) => unreachable!(),
        Knot(_) | Unknown | Future => unreachable!(),
    }
}

fn pp_label<'a>(id: &'a SharedLabel, is_variant: bool, vis: &'a str) -> RcDoc<'a> {
    let vis = if vis.is_empty() {
        RcDoc::nil()
    } else {
        kwd(vis)
    };
    match &**id {
        Label::Named(id) => {
            let case = if is_variant { Some(Case::Pascal) } else { None };
            let (doc, is_rename) = ident_(id, case);
            if is_rename {
                str("#[serde(rename=\"")
                    .append(id.escape_debug().to_string())
                    .append("\")]")
                    .append(RcDoc::line())
                    .append(vis)
                    .append(doc)
            } else {
                vis.append(doc)
            }
        }
        Label::Id(n) | Label::Unnamed(n) => vis.append("_").append(RcDoc::as_string(n)).append("_"),
    }
}

fn pp_record_field<'a>(field: &'a Field, recs: &RecPoints, vis: &'a str) -> RcDoc<'a> {
    pp_label(&field.id, false, vis)
        .append(kwd(":"))
        .append(pp_ty(&field.ty, recs))
}

fn pp_record_fields<'a>(fs: &'a [Field], recs: &RecPoints, vis: &'a str) -> RcDoc<'a> {
    if is_tuple(fs) {
        let vis = if vis.is_empty() {
            RcDoc::nil()
        } else {
            kwd(vis)
        };
        let tuple = RcDoc::concat(
            fs.iter()
                .map(|f| vis.clone().append(pp_ty(&f.ty, recs)).append(",")),
        );
        enclose("(", tuple, ")")
    } else {
        let fields = concat(fs.iter().map(|f| pp_record_field(f, recs, vis)), ",");
        enclose_space("{", fields, "}")
    }
}

fn pp_variant_field<'a>(field: &'a Field, recs: &RecPoints) -> RcDoc<'a> {
    match field.ty.as_ref() {
        TypeInner::Null => pp_label(&field.id, true, ""),
        TypeInner::Record(fs) => {
            pp_label(&field.id, true, "").append(pp_record_fields(fs, recs, ""))
        }
        _ => pp_label(&field.id, true, "").append(enclose("(", pp_ty(&field.ty, recs), ")")),
    }
}

fn pp_variant_fields<'a>(fs: &'a [Field], recs: &RecPoints) -> RcDoc<'a> {
    let fields = concat(fs.iter().map(|f| pp_variant_field(f, recs)), ",");
    enclose_space("{", fields, "}")
}

fn pp_defs<'a>(
    config: &'a Config,
    env: &'a TypeEnv,
    def_list: &'a [&'a str],
    recs: &'a RecPoints,
) -> RcDoc<'a> {
    let derive = if config.type_attributes.is_empty() {
        "#[derive(CandidType, Deserialize)]"
    } else {
        &config.type_attributes
    };
    lines(def_list.iter().map(|id| {
        let ty = env.find_type(id).unwrap();
        let name = ident(id, Some(Case::Pascal)).append(" ");
        let vis = "pub ";
        match ty.as_ref() {
            TypeInner::Record(fs) => {
                let separator = if is_tuple(fs) {
                    RcDoc::text(";")
                } else {
                    RcDoc::nil()
                };
                str(derive)
                    .append(RcDoc::line())
                    .append(vis)
                    .append("struct ")
                    .append(name)
                    .append(pp_record_fields(fs, recs, "pub"))
                    .append(separator)
                    .append(RcDoc::hardline())
            }
            TypeInner::Variant(fs) => str(derive)
                .append(RcDoc::line())
                .append(vis)
                .append("enum ")
                .append(name)
                .append(pp_variant_fields(fs, recs))
                .append(RcDoc::hardline()),
            TypeInner::Func(func) => str("candid::define_function!(")
                .append(vis)
                .append(name)
                .append(": ")
                .append(pp_ty_func(func))
                .append(");"),
            TypeInner::Service(serv) => str("candid::define_service!(")
                .append(vis)
                .append(name)
                .append(": ")
                .append(pp_ty_service(serv))
                .append(");"),
            _ => {
                if recs.contains(id) {
                    str(derive)
                        .append(RcDoc::line())
                        .append(vis)
                        .append("struct ")
                        .append(ident(id, Some(Case::Pascal)))
                        .append(enclose("(", pp_ty(ty, recs), ")"))
                        .append(";")
                        .append(RcDoc::hardline())
                } else {
                    str(vis)
                        .append(kwd("type"))
                        .append(name)
                        .append("= ")
                        .append(pp_ty(ty, recs))
                        .append(";")
                }
            }
        }
    }))
}

fn pp_args(args: &[Type]) -> RcDoc {
    let empty = RecPoints::default();
    let doc = concat(args.iter().map(|t| pp_ty(t, &empty)), ",");
    enclose("(", doc, ")")
}
fn pp_ty_func(f: &Function) -> RcDoc {
    let args = pp_args(&f.args);
    let rets = pp_args(&f.rets);
    let modes = candid::pretty::candid::pp_modes(&f.modes);
    args.append(" ->")
        .append(RcDoc::space())
        .append(rets.append(modes))
        .nest(INDENT_SPACE)
}
fn pp_ty_service(serv: &[(String, Type)]) -> RcDoc {
    let doc = concat(
        serv.iter().map(|(id, func)| {
            let func_doc = match func.as_ref() {
                TypeInner::Func(ref f) => enclose("candid::func!(", pp_ty_func(f), ")"),
                TypeInner::Var(_) => pp_ty(func, &RecPoints::default()).append("::ty()"),
                _ => unreachable!(),
            };
            RcDoc::text("\"")
                .append(id)
                .append(kwd("\" :"))
                .append(func_doc)
        }),
        ";",
    );
    enclose_space("{", doc, "}")
}

fn pp_function<'a>(config: &Config, id: &'a str, func: &'a Function) -> RcDoc<'a> {
    let name = ident(id, Some(Case::Snake));
    let empty = BTreeSet::new();

    let arg_prefix = str(match config.target {
        Target::CanisterCall => "&self",
        Target::Agent => "&self",
        Target::CanisterStub => unimplemented!(),
        Target::Builder => "&self",
    });
    let args = concat(
        std::iter::once(arg_prefix).chain(
            func.args
                .iter()
                .enumerate()
                .map(|(i, ty)| RcDoc::as_string(format!("arg{i}: ")).append(pp_ty(ty, &empty))),
        ),
        ",",
    );
    let result = match config.target {
        Target::CanisterCall => enclose(
            "Result<(",
            RcDoc::concat(func.rets.iter().map(|ty| pp_ty(ty, &empty).append(","))),
            ")>",
        ),
        Target::Agent => match func.rets.len() {
            0 => str("Result<()>"),
            1 => enclose("Result<(", pp_ty(&func.rets[0], &empty), ")>"),
            _ => enclose(
                "Result<(",
                RcDoc::intersperse(
                    func.rets.iter().map(|ty| pp_ty(ty, &empty)),
                    RcDoc::text(", "),
                ),
                ")>",
            ),
        },
        Target::CanisterStub => unimplemented!(),
        Target::Builder => match func.rets.len() {
            0 => str("super::CallBuilder<()>"),
            1 => enclose("super::CallBuilder<", pp_ty(&func.rets[0], &empty), ">"),
            _ => enclose(
                "super::CallBuilder<(",
                RcDoc::intersperse(
                    func.rets.iter().map(|ty| pp_ty(ty, &empty)),
                    RcDoc::text(", "),
                ),
                ")>",
            ),
        },
    };
    let sig_prefix = match config.target {
        Target::CanisterCall | Target::Agent | Target::CanisterStub => kwd("pub async fn"),
        Target::Builder => kwd("pub fn"),
    };
    let sig = sig_prefix
        .append(name)
        .append(enclose("(", args, ")"))
        .append(kwd(" ->"))
        .append(result)
        .append(RcDoc::space());
    let method = id.escape_debug().to_string();

    let body = match config.target {
        Target::CanisterCall => {
            let args = RcDoc::concat((0..func.args.len()).map(|i| RcDoc::text(format!("arg{i},"))));
            str("ic_cdk::call(self.0, \"")
                .append(method)
                .append("\", ")
                .append(enclose("(", args, ")"))
                .append(").await")
        }
        Target::Agent => {
            let is_query = func.is_query();
            let builder_method = if is_query { "query" } else { "update" };
            let call = if is_query { "call" } else { "call_and_wait" };
            let args = RcDoc::intersperse(
                (0..func.args.len()).map(|i| RcDoc::text(format!("&arg{i}"))),
                RcDoc::text(", "),
            );
            let blob = str("Encode!").append(enclose("(", args, ")?;"));
            let rets = RcDoc::concat(
                func.rets
                    .iter()
                    .map(|ty| str(", ").append(pp_ty(ty, &empty))),
            );
            str("let args = ").append(blob).append(RcDoc::hardline())
                        .append(format!("let bytes = self.1.{builder_method}(&self.0, \"{method}\").with_arg(args).{call}().await?;"))
                        .append(RcDoc::hardline())
                        .append("Ok(Decode!(&bytes").append(rets).append(")?)")
        }
        Target::CanisterStub => unimplemented!(),
        Target::Builder => {
            let mode = if func.is_query() {
                "super::CallMode::Query"
            } else {
                "super::CallMode::Update"
            };
            let args = RcDoc::intersperse(
                (0..func.args.len()).map(|i| RcDoc::text(format!("&arg{i}"))),
                RcDoc::text(", "),
            );
            str("let args = ")
                .append(str("Encode!").append(enclose("(", args, ");")))
                .append(RcDoc::hardline())
                .append(enclose(
                    "self.caller.call(",
                    RcDoc::intersperse(
                        [
                            str("self.canister_id"),
                            str(mode),
                            enclose("\"", RcDoc::text(method), "\""),
                            str("args"),
                        ],
                        ", ",
                    ),
                    ")",
                ))
        }
    };

    sig.append(enclose_space("{", body, "}"))
}

fn pp_actor<'a>(config: &'a Config, env: &'a TypeEnv, actor: &'a Type) -> RcDoc<'a> {
    // TODO trace to service before we figure out what canister means in Rust

    let init_args = if let TypeInner::Class(args, _) = actor.as_ref() {
        Some(args)
    } else {
        None
    };

    let serv = env.as_service(actor).unwrap();
    let body = RcDoc::intersperse(
        serv.iter().map(|(id, func)| {
            let func = env.as_func(func).unwrap();
            pp_function(config, id, func)
        }),
        RcDoc::hardline(),
    );
    let struct_name = config.service_name.to_case(Case::Pascal);

    let service_def_prefix = RcDoc::text(format!("pub struct {} ", struct_name));

    let service_def_body = match config.target {
        Target::CanisterCall => str("(pub Principal);"),
        Target::Agent => str("(pub Principal, pub &'a ic_agent::Agent);"),
        Target::CanisterStub => unimplemented!(),
        Target::Builder => enclose_space(
            "{",
            RcDoc::intersperse(
                [
                    str("pub canister_id: Principal"),
                    str("pub caller: super::Caller"),
                ],
                str(",").append(RcDoc::hardline()),
            ),
            "}",
        ),
    };

    let service_def = service_def_prefix
        .append(service_def_body)
        .append(RcDoc::hardline());

    let service_impl = match config.target {
        Target::CanisterCall => format!("impl {} ", struct_name),
        Target::Agent => format!("impl<'a> {}<'a> ", struct_name),
        Target::CanisterStub => unimplemented!(),
        Target::Builder => format!("impl {} ", struct_name),
    };
    let res = service_def
        .append(RcDoc::hardline())
        .append(service_impl)
        .append(enclose_space("{", body, "}"))
        .append(RcDoc::hardline());
    let res = if let Some(cid) = config.canister_id {
        let slice = cid
            .as_slice()
            .iter()
            .map(|b| b.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        let id = RcDoc::text(format!(
            "pub const CANISTER_ID : Principal = Principal::from_slice(&[{}]); // {}",
            slice, cid
        ));
        let instance = match config.target {
            Target::CanisterCall => format!(
                "pub const {} : {} = {}(CANISTER_ID);",
                config.service_name, struct_name, struct_name
            ),
            Target::Agent => "".to_string(),
            Target::CanisterStub => unimplemented!(),
            Target::Builder => "".to_string(),
        };
        res.append(id).append(RcDoc::hardline()).append(instance)
    } else {
        res
    };

    res.append(pp_actor_new(config, struct_name.clone()))
        .append(pp_actor_deploy(config, struct_name, init_args))
        .append(pp_actor_canister_id(config))
        .append(pp_actor_wasm(config))
}

pub fn pp_actor_new<'a>(config: &'a Config, struct_name: String) -> RcDoc<'a> {
    match config.target {
        Target::CanisterCall | Target::Agent | Target::CanisterStub => RcDoc::nil(),
        Target::Builder => {
            let args = RcDoc::intersperse(
                [str("caller: &super::Caller"), str("canister_id: Principal")],
                ", ",
            );
            let body = RcDoc::text(struct_name.clone())
                .append(RcDoc::space())
                .append(enclose_space(
                    "{",
                    RcDoc::intersperse(
                        [str("canister_id"), str("caller: caller.clone()")],
                        str(",").append(RcDoc::line()),
                    ),
                    "}",
                ));
            let result = enclose("pub fn new(", args, ")")
                .append(format!("-> {}", struct_name))
                .append(RcDoc::space())
                .append(enclose_space("{", body, "}"))
                .append(RcDoc::hardline());
            RcDoc::hardline().append(result)
        }
    }
}

pub fn pp_actor_deploy<'a>(
    config: &'a Config,
    struct_name: String,
    init_args: Option<&'a Vec<Type>>,
) -> RcDoc<'a> {
    let Some(init_args) = init_args else {
        return RcDoc::nil();
    };

    let empty = BTreeSet::new();

    match config.target {
        Target::CanisterCall | Target::Agent | Target::CanisterStub => RcDoc::nil(),
        Target::Builder => {
            let args = RcDoc::intersperse(
                std::iter::once(str("deployer: &super::Deployer")).chain(
                    init_args.iter().enumerate().map(|(i, ty)| {
                        RcDoc::as_string(format!("arg{i}: ")).append(pp_ty(ty, &empty))
                    }),
                ),
                ", ",
            );

            let sig = enclose("pub fn deploy(", args, ")")
                .append(format!("-> super::DeployBuilder<{}>", struct_name))
                .append(RcDoc::space());

            let args = RcDoc::intersperse(
                (0..init_args.len()).map(|i| RcDoc::text(format!("&arg{i}"))),
                RcDoc::text(", "),
            );

            let body = str("let args = ")
                .append(str("Encode!").append(enclose("(", args, ");")))
                .append(RcDoc::hardline())
                .append(str("let result = "))
                .append(enclose(
                    "deployer.deploy(",
                    RcDoc::intersperse([str("args"), str("new")], ", "),
                    ");",
                ))
                .append(RcDoc::hardline())
                .append(str("let result = if let Some(id) = canister_id()"))
                .append(enclose("{", str("result.with_canister_id(id)"), "}"))
                .append(enclose("else {", str("result"), "};"))
                .append(RcDoc::hardline())
                .append(str("if let Some(wasm) = wasm()"))
                .append(enclose("{", str("result.with_wasm(wasm)"), "}"))
                .append(enclose("else {", str("result"), "}"));

            let result = sig
                .append(RcDoc::hardline())
                .append(enclose_space("{", body, "}"));

            RcDoc::hardline().append(result)
        }
    }
}

pub fn pp_actor_canister_id<'a>(config: &'a Config) -> RcDoc<'a> {
    match config.target {
        Target::CanisterCall | Target::Agent | Target::CanisterStub => RcDoc::nil(),
        Target::Builder => {
            let body = if let Some(canister_id) = config.canister_id {
                RcDoc::text(format!(
                    "Some(Principal::from_text(\"{}\").unwrap())",
                    canister_id
                ))
            } else {
                str("None")
            };

            let result = str("pub fn canister_id() -> Option<Principal>")
                .append(RcDoc::space())
                .append(enclose_space("{", body, "}"))
                .append(RcDoc::hardline());
            RcDoc::hardline().append(result)
        }
    }
}

pub fn pp_actor_wasm<'a>(config: &'a Config) -> RcDoc<'a> {
    match config.target {
        Target::CanisterCall | Target::Agent | Target::CanisterStub => RcDoc::nil(),
        Target::Builder => {
            let body = if let Some(wasm_path) = config.canister_wasm_path.clone() {
                let path = if wasm_path.starts_with("$HOME") {
                    let wasm_path = wasm_path[5..].to_string();
                    str("let mut path = std::path::PathBuf::new();")
                        .append(str("path.push(std::env::var(\"HOME\").unwrap());"))
                        .append(RcDoc::text(format!("path.push(\"{}\");", wasm_path)))
                } else {
                    str("let mut path = std::path::PathBuf::new();")
                        .append(RcDoc::text(format!("path.push(\"{}\");", wasm_path)))
                };

                path.append(str("let wasm = std::fs::read(path.as_path())"))
                    .append(".unwrap_or_else")
                    .append(enclose(
                        "(",
                        str("|_| panic!(\"wasm binary not found: {:?}\", path)"),
                        ");",
                    ))
                    .append(str("Some(wasm)"))
            } else {
                str("None")
            };
            let result = str("pub fn wasm() -> Option<Vec<u8>>")
                .append(RcDoc::space())
                .append(enclose_space("{", body, "}"))
                .append(RcDoc::hardline());
            RcDoc::hardline().append(result)
        }
    }
}

pub fn compile(config: &Config, env: &TypeEnv, actor: &Option<Type>) -> String {
    let header = format!(
        r#"// This is an experimental feature to generate Rust binding from Candid.
// You may want to manually adjust some of the types.
#![allow(dead_code, unused_imports, non_snake_case)]
use {}::{{self, CandidType, Deserialize, Principal, Encode, Decode}};
"#,
        config.candid_crate
    );
    let header = header
        + match &config.target {
            Target::CanisterCall => "use ic_cdk::api::call::CallResult as Result;\n",
            Target::Agent => "type Result<T> = std::result::Result<T, ic_agent::AgentError>;\n",
            Target::CanisterStub => "",
            Target::Builder => "",
        };
    let (env, actor) = nominalize_all(env, actor);
    let def_list: Vec<_> = if let Some(actor) = &actor {
        chase_actor(&env, actor).unwrap()
    } else {
        env.0.iter().map(|pair| pair.0.as_ref()).collect()
    };
    let recs = infer_rec(&env, &def_list).unwrap();
    let defs = pp_defs(config, &env, &def_list, &recs);
    let doc = match &actor {
        None => defs,
        Some(actor) => {
            let actor = pp_actor(config, &env, actor);
            defs.append(actor)
        }
    };
    let doc = RcDoc::text(header).append(RcDoc::line()).append(doc);
    doc.pretty(LINE_WIDTH).to_string()
}

pub enum TypePath {
    Id(String),
    Opt,
    Vec,
    RecordField(String),
    VariantField(String),
    Func(String),
    Init,
}
fn path_to_var(path: &[TypePath]) -> String {
    let name: Vec<&str> = path
        .iter()
        .map(|node| match node {
            TypePath::Id(id) => id.as_str(),
            TypePath::RecordField(f) | TypePath::VariantField(f) => f.as_str(),
            TypePath::Opt => "inner",
            TypePath::Vec => "item",
            TypePath::Func(id) => id.as_str(),
            TypePath::Init => "init",
        })
        .collect();
    name.join("_").to_case(Case::Pascal)
}
// Convert structural typing to nominal typing to fit Rust's type system
fn nominalize(env: &mut TypeEnv, path: &mut Vec<TypePath>, t: &Type) -> Type {
    match t.as_ref() {
        TypeInner::Opt(ty) => {
            path.push(TypePath::Opt);
            let ty = nominalize(env, path, ty);
            path.pop();
            TypeInner::Opt(ty)
        }
        TypeInner::Vec(ty) => {
            path.push(TypePath::Vec);
            let ty = nominalize(env, path, ty);
            path.pop();
            TypeInner::Vec(ty)
        }
        TypeInner::Record(fs) => {
            if matches!(
                path.last(),
                None | Some(TypePath::VariantField(_)) | Some(TypePath::Id(_))
            ) || is_tuple(fs)
            {
                let fs: Vec<_> = fs
                    .iter()
                    .map(|Field { id, ty }| {
                        path.push(TypePath::RecordField(id.to_string()));
                        let ty = nominalize(env, path, ty);
                        path.pop();
                        Field { id: id.clone(), ty }
                    })
                    .collect();
                TypeInner::Record(fs)
            } else {
                let new_var = path_to_var(path);
                let ty = nominalize(
                    env,
                    &mut vec![TypePath::Id(new_var.clone())],
                    &TypeInner::Record(fs.to_vec()).into(),
                );
                env.0.insert(new_var.clone(), ty);
                TypeInner::Var(new_var)
            }
        }
        TypeInner::Variant(fs) => match path.last() {
            None | Some(TypePath::Id(_)) => {
                let fs: Vec<_> = fs
                    .iter()
                    .map(|Field { id, ty }| {
                        path.push(TypePath::VariantField(id.to_string()));
                        let ty = nominalize(env, path, ty);
                        path.pop();
                        Field { id: id.clone(), ty }
                    })
                    .collect();
                TypeInner::Variant(fs)
            }
            Some(_) => {
                let new_var = path_to_var(path);
                let ty = nominalize(
                    env,
                    &mut vec![TypePath::Id(new_var.clone())],
                    &TypeInner::Variant(fs.to_vec()).into(),
                );
                env.0.insert(new_var.clone(), ty);
                TypeInner::Var(new_var)
            }
        },
        TypeInner::Func(func) => match path.last() {
            None | Some(TypePath::Id(_)) => {
                let func = func.clone();
                TypeInner::Func(Function {
                    modes: func.modes,
                    args: func
                        .args
                        .into_iter()
                        .enumerate()
                        .map(|(i, ty)| {
                            let i = if i == 0 {
                                "".to_string()
                            } else {
                                i.to_string()
                            };
                            path.push(TypePath::Func(format!("arg{i}")));
                            let ty = nominalize(env, path, &ty);
                            path.pop();
                            ty
                        })
                        .collect(),
                    rets: func
                        .rets
                        .into_iter()
                        .enumerate()
                        .map(|(i, ty)| {
                            let i = if i == 0 {
                                "".to_string()
                            } else {
                                i.to_string()
                            };
                            path.push(TypePath::Func(format!("ret{i}")));
                            let ty = nominalize(env, path, &ty);
                            path.pop();
                            ty
                        })
                        .collect(),
                })
            }
            Some(_) => {
                let new_var = path_to_var(path);
                let ty = nominalize(
                    env,
                    &mut vec![TypePath::Id(new_var.clone())],
                    &TypeInner::Func(func.clone()).into(),
                );
                env.0.insert(new_var.clone(), ty);
                TypeInner::Var(new_var)
            }
        },
        TypeInner::Service(serv) => match path.last() {
            None | Some(TypePath::Id(_)) => TypeInner::Service(
                serv.iter()
                    .map(|(meth, ty)| {
                        path.push(TypePath::Id(meth.to_string()));
                        let ty = nominalize(env, path, ty);
                        path.pop();
                        (meth.clone(), ty)
                    })
                    .collect(),
            ),
            Some(_) => {
                let new_var = path_to_var(path);
                let ty = nominalize(
                    env,
                    &mut vec![TypePath::Id(new_var.clone())],
                    &TypeInner::Service(serv.clone()).into(),
                );
                env.0.insert(new_var.clone(), ty);
                TypeInner::Var(new_var)
            }
        },
        TypeInner::Class(args, ty) => TypeInner::Class(
            args.iter()
                .map(|ty| {
                    path.push(TypePath::Init);
                    let ty = nominalize(env, path, ty);
                    path.pop();
                    ty
                })
                .collect(),
            nominalize(env, path, ty),
        ),
        _ => return t.clone(),
    }
    .into()
}

fn nominalize_all(env: &TypeEnv, actor: &Option<Type>) -> (TypeEnv, Option<Type>) {
    let mut res = TypeEnv(Default::default());
    for (id, ty) in env.0.iter() {
        let ty = nominalize(&mut res, &mut vec![TypePath::Id(id.clone())], ty);
        res.0.insert(id.to_string(), ty);
    }
    let actor = actor
        .as_ref()
        .map(|ty| nominalize(&mut res, &mut vec![], ty));
    (res, actor)
}
