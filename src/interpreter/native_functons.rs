use std::time::SystemTime;

use super::{value::Value, value::CallableType, environment::Environment};


fn clock(_args: &Vec<Value>) -> Value {
    let now = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap();
    Value::Number(now.as_secs() as f64)
}

const CLOCK: CallableType = CallableType::Native{
    name: "clock",
    arity: 0,
    call: clock,
};

pub fn inject_native_functions(env: &mut Environment) {
    for native_functon in [
        CLOCK,
    ] {
        match native_functon {
            CallableType::Native { name, arity: _, call: _ } =>
                env.set_in_current_scope(name.to_owned(), Value::Callable(native_functon)),
            CallableType::Function { name: _, parameters: _, body: _, position: _  } => unreachable!("Lox functions cannot be injected"),
        };
    }
}
