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

/*

                 +---------+-------+
                 | name-id | value |
 partial-ord-id  +---------+-------+
      ^                 \   /
      |                  \ /
    +-|-+   +---+---+---+-V-+---+
    | * |   |   |   |   | * |   | ...  ---------> stack
    +---+   +---+---+---+---+---+
    |   |   |   |   | ...  ---------------------> stack
    +---+   +---+---+---+---+---+---+
    |   |   |   |   |   |   |   |   | ...  -----> stack
    +---+   +---+---+---+---+---+---+
    |   |   |   |   |   |   | ...  -------------> stack
    +---+   +---+---+---+---+
     ...


We have a sequence of PartialOrd which keeps track of processes. Each id
(PartialOrd) has an associated stack, where variables are stored; two way of
storing variables:
  1) unboxed;
  2) boxed;

It must hold that, for each stored PartialOrd a,b, let i and j be their indexes
in the sequence respectively, if i < j, then a is NOT greater than b (it could
be ancestor or match exactly or nothing of them).

Processes are just represented by PartialOrd, but it is not required that a
single process (whatever it means, here there isn't the notion of it) is
associated to a single PartialOrd.
*/
pub struct NamesStack<T /*: PartialOrd*/> {
    bindings: Vec<(T, Vec<NameBinding>)>,
}

impl<T> NamesStack<T> {
    const DEFAULT_NAMES_CAPACITY: usize = 16;

    pub fn new() -> Self {
        NamesStack {
            bindings: Vec::with_capacity(Self::DEFAULT_NAMES_CAPACITY),
        }
    }
}

impl<T: PartialOrd> NamesStack<T> {
    pub fn push(&mut self, pid: T, name_id: NameId, value: Value) -> bool {
        todo!()
    }

    pub fn push_channel(&mut self, pid: T, name_id: NameId) -> bool {
        todo!()
    }

    //TODO: add update op

    pub fn lookup(&self, pid: T, name_id: NameId) -> Option<Value> {
        todo!()
        //self.bindings
        //    .iter()
        //    .rfind(|&(n, _)| *n == name_id)
        //    .map(|(_, v)| v.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push() {
        let name_id1 = String::from("n1");
        let name_id2 = String::from("n2");
        let name_id3 = String::from("n3");
        let name_id4 = String::from("n4");
        let name_id5 = String::from("n5");
        let v1 = Value::I32(42);
        let v2 = Value::I32(43);
        let v3 = Value::I32(7);
        let pid1 = 100;
        let pid2 = 200;
        let pid3 = 300;
        let pid4 = 50;
        let pid5 = 150;
        let pid6 = 250;

        let mut stack = NamesStack {
            bindings: Vec::new(),
        };

        assert!(stack.push(pid1, name_id1.clone(), v1.clone()));
        assert!(stack.push(pid1, name_id2.clone(), v2.clone()));

        assert!(!stack.push(pid4, name_id1.clone(), v1.clone()));

        assert_eq!(stack.bindings.len(), 1);
        assert_eq!(stack.bindings[0].0, pid1);
        assert_eq!(stack.bindings[0].1.len(), 2);

        assert_eq!(stack.bindings[0].1[0].1, v1);
        assert_eq!(stack.bindings[0].1[1].1, v2);
        assert_eq!(stack.bindings[0].1[0].0, name_id1);
        assert_eq!(stack.bindings[0].1[1].0, name_id2);

        assert!(stack.push(pid2, name_id3.clone(), v1.clone()));

        assert!(!stack.push(pid4, name_id1.clone(), v1.clone()));
        assert!(!stack.push(pid5, name_id1.clone(), v1.clone()));

        assert_eq!(stack.bindings.len(), 2);
        assert_eq!(stack.bindings[0].1.len(), 2);
        assert_eq!(stack.bindings[1].1[0].1, v1);
        assert_eq!(stack.bindings[1].1[0].0, name_id3);

        assert!(stack.push(pid3, name_id5.clone(), v1.clone()));
        assert!(stack.push(pid1, name_id4.clone(), v1.clone()));
        assert!(stack.push(pid3, name_id2.clone(), v3.clone()));
        assert!(stack.push(pid3, name_id3.clone(), v3.clone()));

        assert!(!stack.push(pid4, name_id1.clone(), v1.clone()));
        assert!(!stack.push(pid5, name_id1.clone(), v1.clone()));
        assert!(!stack.push(pid6, name_id1.clone(), v1.clone()));

        assert_eq!(stack.bindings.len(), 3);
        assert_eq!(stack.bindings[0].0, pid1);
        assert_eq!(stack.bindings[1].0, pid2);
        assert_eq!(stack.bindings[2].0, pid3);

        assert_eq!(stack.bindings[0].1.len(), 3);
        assert_eq!(stack.bindings[1].1.len(), 1);
        assert_eq!(stack.bindings[2].1.len(), 3);

        assert_eq!(stack.bindings[0].1[0].1, v1);
        assert_eq!(stack.bindings[0].1[1].1, v2);
        assert_eq!(stack.bindings[0].1[2].1, v1);
        assert_eq!(stack.bindings[0].1[0].0, name_id1);
        assert_eq!(stack.bindings[0].1[1].0, name_id2);
        assert_eq!(stack.bindings[0].1[2].0, name_id4);

        assert_eq!(stack.bindings[1].1[0].1, v1);
        assert_eq!(stack.bindings[1].1[0].0, name_id3);

        assert_eq!(stack.bindings[2].1[0].1, v1);
        assert_eq!(stack.bindings[2].1[1].1, v3);
        assert_eq!(stack.bindings[2].1[2].1, v3);
        assert_eq!(stack.bindings[2].1[0].0, name_id5);
        assert_eq!(stack.bindings[2].1[1].0, name_id2);
        assert_eq!(stack.bindings[2].1[2].0, name_id3);
    }

    #[test]
    fn test_lookup() {
        let name_id1 = String::from("n1");
        let name_id2 = String::from("n2");
        let name_id3 = String::from("n3");
        let name_id4 = String::from("n4");
        let name_id5 = String::from("n5");
        let v1 = Value::I32(42);
        let v2 = Value::I32(43);
        let v3 = Value::I32(7);
        let pid1 = 100;
        let pid2 = 200;
        let pid3 = 300;
        let pid4 = 400;
        let pid5 = 500;
        let pid6 = 50;
        let pid7 = 150;

        let mut stack = NamesStack {
            bindings: Vec::new(),
        };

        stack.push(pid1, name_id1.clone(), v1.clone());
        stack.push(pid1, name_id2.clone(), v2.clone());

        assert_eq!(stack.lookup(pid1, name_id1.clone()), Some(v1.clone()));
        assert_eq!(stack.lookup(pid1, name_id3.clone()), None);
        assert_eq!(stack.lookup(pid1, name_id2.clone()), Some(v2.clone()));
        assert_eq!(stack.lookup(pid2, name_id2.clone()), None);

        stack.push(pid6, name_id1.clone(), v1.clone());

        assert_eq!(stack.lookup(pid6, name_id1.clone()), None);

        stack.push(pid2, name_id1.clone(), v2.clone());

        assert_eq!(stack.lookup(pid1, name_id1.clone()), Some(v1.clone()));
        assert_eq!(stack.lookup(pid2, name_id1.clone()), Some(v2.clone()));
        assert_eq!(stack.lookup(pid3, name_id1.clone()), Some(v2.clone()));
        assert_eq!(stack.lookup(pid4, name_id1.clone()), Some(v2.clone()));
        assert_eq!(stack.lookup(pid5, name_id1.clone()), Some(v2.clone()));
        assert_eq!(stack.lookup(pid6, name_id1.clone()), None);
        assert_eq!(stack.lookup(pid7, name_id1.clone()), Some(v1.clone()));

        stack.push(pid1, name_id3.clone(), v3.clone());
        stack.push(pid4, name_id4.clone(), v2.clone());
        stack.push(pid4, name_id2.clone(), v3.clone());

        assert_eq!(stack.lookup(pid1, name_id3.clone()), Some(v3.clone()));
        assert_eq!(stack.lookup(pid2, name_id3.clone()), Some(v3.clone()));
        assert_eq!(stack.lookup(pid3, name_id3.clone()), Some(v3.clone()));
        assert_eq!(stack.lookup(pid4, name_id3.clone()), Some(v3.clone()));
        assert_eq!(stack.lookup(pid5, name_id3.clone()), Some(v3.clone()));
        assert_eq!(stack.lookup(pid6, name_id3.clone()), None);
        assert_eq!(stack.lookup(pid7, name_id3.clone()), Some(v3.clone()));

        assert_eq!(stack.lookup(pid1, name_id4.clone()), None);
        assert_eq!(stack.lookup(pid2, name_id4.clone()), None);
        assert_eq!(stack.lookup(pid3, name_id4.clone()), None);
        assert_eq!(stack.lookup(pid4, name_id4.clone()), Some(v2.clone()));
        assert_eq!(stack.lookup(pid5, name_id4.clone()), Some(v2.clone()));
        assert_eq!(stack.lookup(pid6, name_id4.clone()), None);
        assert_eq!(stack.lookup(pid7, name_id4.clone()), None);

        assert_eq!(stack.lookup(pid1, name_id2.clone()), Some(v2.clone()));
        assert_eq!(stack.lookup(pid2, name_id2.clone()), Some(v2.clone()));
        assert_eq!(stack.lookup(pid3, name_id2.clone()), Some(v2.clone()));
        assert_eq!(stack.lookup(pid4, name_id2.clone()), Some(v3.clone()));
        assert_eq!(stack.lookup(pid5, name_id2.clone()), Some(v3.clone()));
        assert_eq!(stack.lookup(pid6, name_id4.clone()), None);
        assert_eq!(stack.lookup(pid7, name_id2.clone()), Some(v2.clone()));

        assert_eq!(stack.lookup(pid1, name_id5.clone()), None);
        assert_eq!(stack.lookup(pid2, name_id5.clone()), None);
        assert_eq!(stack.lookup(pid3, name_id5.clone()), None);
        assert_eq!(stack.lookup(pid4, name_id5.clone()), None);
        assert_eq!(stack.lookup(pid5, name_id5.clone()), None);
        assert_eq!(stack.lookup(pid6, name_id5.clone()), None);
        assert_eq!(stack.lookup(pid7, name_id5.clone()), None);
    }

    #[test]
    fn test_shadowing_single_pid() {
        //TODO
    }
}
