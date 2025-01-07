pub type NameId = String;
// NB: if you add cases to this type, remember to box everything is large
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Value {
    I32(i32),
    Channel,
}
pub type NameBinding = (NameId, Value);

pub fn nameIdOf(binding: NameBinding) -> NameId {
    binding.0
}

pub fn valueOf(binding: NameBinding) -> Value {
    binding.1
}

pub type ActorId = String;

/*
               +---------+-------+
               | name-id | value |
               +---------+-------+
                      \   /
                       \ /
                      +-V-+---+---+---+---+---+---+---+---+
bindings              | * |   |   |   |   |   |   |   |   | ...
(the effective stack) +---+---+---+---+---+---+---+---+---+


     actor-id             index
         ^                  ^
         |                  |
       +-|-+  +---+---+---+-|-+---+---+---+---+
scopes | * |  |   |   |   | * |   |   |   |   | ...
       +---+  +---+---+---+---+---+---+---+---+
       |   |  |   |   |   |   |   |   |   |   |
       +---+  +---+---+---+---+---+---+---+---+
       |   |  |   |   |   |   |   |   |   |   |
       +---+  +---+---+---+---+---+---+---+---+
       |   |  |   |   |   |   |   |   |   |   |
       +---+  +---+---+---+---+---+---+---+---+
           ...

`bindings` are the real stack where variables are stored; two way of storing
variables:
  1) unboxed;
  2) boxed;

`scopes` are a table to trace which variables the processes can see. The
processes are represented by actor-ids (which don't necessarily correspond to
process-ids). Each actor-id is associated with a set of indexes which are the
ones that are visible in the stack to the actor.
*/
pub struct NamesStack {
    bindings: Vec<NameBinding>,
    scopes: Vec<(ActorId, Vec<i32>)>,
}

impl NamesStack {
    const DEFAULT_NAMES_CAPACITY: usize = 16;
    const DEFAULT_SCOPES_CAPACITY: usize = 16;

    pub fn new() -> Self {
        NamesStack {
            bindings: Vec::with_capacity(Self::DEFAULT_NAMES_CAPACITY),
            scopes: Vec::with_capacity(Self::DEFAULT_SCOPES_CAPACITY),
        }
    }

    pub fn push(&mut self, name_id: NameId, value: Value) {
        self.bindings.push((name_id, value))
    }

    pub fn push_channel(&mut self, name_id: NameId) {
        self.bindings.push((name_id, Value::Channel))
    }

    //TODO: add update op

    pub fn pop(&mut self) -> Option<NameBinding> {
        self.bindings.pop()
    }

    pub fn lookup(&self, name_id: NameId) -> Option<Value> {
        self.bindings
            .iter()
            .rfind(|&(n, _)| *n == name_id)
            .map(|(_, v)| v.clone())
    }
}

#[cfg(test)]
mod tests {
    use crate::interpreter::context::{NamesStack, Value};

    #[test]
    fn test_push_pop() {
        let mut stack = NamesStack::new();

        stack.push("key1".to_string(), Value::I32(10));
        stack.push("key2".to_string(), Value::Channel);

        assert_eq!(stack.pop(), Some(("key2".to_string(), Value::Channel)));
        assert_eq!(stack.pop(), Some(("key1".to_string(), Value::I32(10))));
        assert_eq!(stack.pop(), None);
    }

    #[test]
    fn test_lookup() {
        let mut stack = NamesStack::new();
        stack.push("key1".to_string(), Value::I32(10));
        stack.push("key2".to_string(), Value::Channel);
        stack.push("key3".to_string(), Value::I32(30));
        stack.push("key1".to_string(), Value::I32(40));

        assert_eq!(stack.lookup("key1".to_string()), Some(Value::I32(40)));
        assert_eq!(stack.lookup("key2".to_string()), Some(Value::Channel));
        assert_eq!(stack.lookup("key3".to_string()), Some(Value::I32(30)));
        assert_eq!(stack.lookup("key4".to_string()), None);
    }

    #[test]
    fn test_push_pop_lookup() {
        let mut stack = NamesStack::new();

        stack.push("key1".to_string(), Value::I32(10));
        stack.push("key2".to_string(), Value::Channel);
        stack.push("key3".to_string(), Value::I32(30));

        assert_eq!(stack.lookup("key2".to_string()), Some(Value::Channel));
        assert_eq!(stack.pop(), Some(("key3".to_string(), Value::I32(30))));
        assert_eq!(stack.lookup("key2".to_string()), Some(Value::Channel));
        assert_eq!(stack.pop(), Some(("key2".to_string(), Value::Channel)));
        assert_eq!(stack.lookup("key1".to_string()), Some(Value::I32(10)));
        assert_eq!(stack.pop(), Some(("key1".to_string(), Value::I32(10))));
        assert_eq!(stack.pop(), None);
    }
}
