use std::{fmt::Display, ops::Deref};

use tokio::sync::broadcast;

pub type NameId = String;
pub type ChannelData = Box<broadcast::Sender<Value>>;
// NB: if you add cases to this type, remember to box everything is large
#[derive(Debug, Clone)]
pub enum Value {
    I32(i32),
    Channel(ChannelData),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::I32(n) => write!(f, "{}", n),
            Value::Channel(_) => write!(f, "@"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::I32(n1), Value::I32(n2)) => n1 == n2,
            (Value::I32(_), Value::Channel(_)) => false,
            (Value::Channel(_), Value::I32(_)) => false,
            (Value::Channel(_), Value::Channel(_)) => false,
        }
    }
}

impl Value {
    pub fn is_channel(&self) -> bool {
        match self {
            Value::Channel(_) => true,
            _ => false,
        }
    }
}

/*
impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Value::I32(n) => Value::I32(*n),
            Value::Channel(boxed_sender_receiver) => {
                let sender = boxed_sender_receiver.deref();
                Value::Channel(Box::new(sender.clone()))
            }
        }
    }
}
    */

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

impl<T: ToString> NamesStack<T> {
    pub fn draw(&self) -> String {
        self.bindings
            .iter()
            .map(|binding| {
                let head = ["[", binding.0.to_string().as_str(), "] ---> "].concat();
                let sub_stack = binding.1.iter().fold("|".to_owned(), |acc, pair| {
                    [
                        acc.as_str(),
                        " <",
                        pair.0.as_str(),
                        ",",
                        pair.1.to_string().as_str(),
                        "> |",
                    ]
                    .concat()
                });

                [head, sub_stack, "\n".to_owned()].concat()
            })
            .collect()
    }
}

impl<T: PartialOrd> NamesStack<T> {
    const DEFAULT_SUB_STACK_CAPACITY: usize = 4;

    pub fn push_pid(&mut self, pid: T) -> bool {
        if self.bindings.iter().any(|sub_stack| sub_stack.0 >= pid) {
            false
        } else {
            let new_stack = Vec::with_capacity(Self::DEFAULT_SUB_STACK_CAPACITY);
            self.bindings.push((pid, new_stack));
            true
        }
    }

    pub fn push(&mut self, pid: T, name_id: NameId, value: Value) -> bool {
        // Little optimization in order to traverse the whole vector once at
        // maximum. The alternative would be to check if any element is greater
        // than the input pid with another traverse.
        let mut any_greater = false;
        match self.bindings.iter_mut().rev().find(|sub_stack| {
            if sub_stack.0 > pid {
                any_greater = true
            }
            sub_stack.0 == pid
        }) {
            Some(sub_stack) => {
                // TODO: handle shadowing inside single stack
                if sub_stack.1.iter().any(|binding| binding.0 == name_id) {
                    false
                } else {
                    sub_stack.1.push((name_id, value));
                    true
                }
            }
            None => {
                if any_greater {
                    false
                } else {
                    let mut new_stack = Vec::with_capacity(Self::DEFAULT_SUB_STACK_CAPACITY);
                    new_stack.push((name_id, value));
                    self.bindings.push((pid, new_stack));
                    true
                }
            }
        }
    }

    pub fn push_channel(&mut self, pid: T, name_id: NameId) -> bool {
        let (sender, _) = broadcast::channel(16); //TODO: remove hardcoding
        self.push(pid, name_id, Value::Channel(Box::new(sender)))
    }

    //TODO: add update op

    pub fn lookup(&self, pid: T, name_id: NameId) -> Option<Value> {
        self.lookup_with(pid, &|binding| binding.0 == name_id)
    }

    pub fn lookup_channel(&self, pid: T, name_id: NameId) -> Option<Value> {
        self.lookup_with(pid, &|binding| {
            binding.0 == name_id && binding.1.is_channel()
        })
    }

    fn lookup_with<F>(&self, pid: T, by_predicate: &F) -> Option<Value>
    where
        F: Fn(&&(NameId, Value)) -> bool,
    {
        let mut iter = self.bindings.iter().rev();
        loop {
            // Searching in the pid vector for the pid firstly, mutating the iterator
            match iter.find(|sub_stack| sub_stack.0 <= pid) {
                // Then searching in the stack for the binding
                Some(sub_stack) => match sub_stack.1.iter().find(by_predicate) {
                    // This clone should be light-weight
                    Some(binding) => return Some(binding.1.clone()),
                    None => (), // do nothing, continue to search
                },
                None => return None,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use broadcast::Sender;
    use pest::prec_climber::Assoc;

    use super::*;

    #[test]
    fn test_push_pid() {
        let pid1 = 100;
        let pid2 = 200;
        let pid3 = 300;
        let pid4 = 50;
        let pid5 = 150;
        let pid6 = 250;

        let mut stack = NamesStack::new();

        assert!(stack.bindings.len() == 0);

        assert!(stack.push_pid(pid1));

        assert!(stack.bindings.len() == 1);
        assert!(stack.bindings[0].0 == pid1);

        assert!(!stack.push_pid(pid1));

        assert!(stack.bindings.len() == 1);
        assert!(stack.bindings[0].0 == pid1);

        assert!(!stack.push_pid(pid4));

        assert!(stack.bindings.len() == 1);
        assert!(stack.bindings[0].0 == pid1);

        assert!(stack.push_pid(pid6));

        assert!(stack.bindings.len() == 2);
        assert!(stack.bindings[0].0 == pid1);
        assert!(stack.bindings[1].0 == pid6);

        assert!(!stack.push_pid(pid4));
        assert!(!stack.push_pid(pid5));
        assert!(!stack.push_pid(pid2));

        assert!(stack.bindings.len() == 2);
        assert!(stack.bindings[0].0 == pid1);
        assert!(stack.bindings[1].0 == pid6);

        assert!(stack.push_pid(pid3));

        assert!(stack.bindings.len() == 3);
        assert!(stack.bindings[0].0 == pid1);
        assert!(stack.bindings[1].0 == pid6);
        assert!(stack.bindings[2].0 == pid3);

        assert!(!stack.push_pid(pid3));
        assert!(!stack.push_pid(pid1));

        assert!(stack.bindings.len() == 3);
        assert!(stack.bindings[0].0 == pid1);
        assert!(stack.bindings[1].0 == pid6);
        assert!(stack.bindings[2].0 == pid3);

        assert!(!stack.push_pid(pid4));
        assert!(!stack.push_pid(pid5));
        assert!(!stack.push_pid(pid6));
        assert!(!stack.push_pid(pid2));

        assert!(stack.bindings.len() == 3);
        assert!(stack.bindings[0].0 == pid1);
        assert!(stack.bindings[1].0 == pid6);
        assert!(stack.bindings[2].0 == pid3);
    }

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
        assert_eq!(stack.lookup(pid2, name_id2.clone()), Some(v2.clone()));
        assert_eq!(stack.lookup(pid6, name_id1.clone()), None);

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

    #[test]
    fn draw_stack_trace() {
        let mut stack = NamesStack::new();

        let name_id1 = String::from("n1");
        let name_id2 = String::from("n2");
        let name_id3 = String::from("n3");
        let name_id4 = String::from("n4");
        let name_id5 = String::from("n5");
        let v1 = Value::I32(42);
        let v2 = Value::I32(43);
        let v3 = Value::I32(7);
        let v4 = Value::Channel(Box::new(Sender::new(4)));
        let v5 = Value::Channel(Box::new(Sender::new(5)));
        let pid1 = 100;
        let pid2 = 200;
        let pid3 = 300;

        let mut drawing = stack.draw();
        println!("{}", drawing);

        stack.push(pid1, name_id1.clone(), v1.clone());

        drawing = stack.draw();
        println!("{}", drawing);

        stack.push(pid2, name_id1.clone(), v2.clone());
        stack.push(pid3, name_id1.clone(), v3.clone());

        drawing = stack.draw();
        println!("{}", drawing);

        stack.push(pid1, name_id2.clone(), v3.clone());
        stack.push(pid2, name_id2.clone(), v4.clone());

        drawing = stack.draw();
        println!("{}", drawing);

        stack.push(pid1, name_id3.clone(), v4.clone());
        stack.push(pid2, name_id3.clone(), v5.clone());

        drawing = stack.draw();
        println!("{}", drawing);

        stack.push(pid3, name_id1.clone(), v3.clone());
        stack.push(pid3, name_id2.clone(), v4.clone());
        stack.push(pid3, name_id3.clone(), v2.clone());
        stack.push(pid3, name_id4.clone(), v5.clone());
        stack.push(pid3, name_id5.clone(), v4.clone());

        drawing = stack.draw();
        println!("{}", drawing);
    }
}
