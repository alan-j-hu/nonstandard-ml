use bumpalo::collections::String;
use bumpalo::Bump;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub struct StringPool<'a> {
    bump: &'a Bump,
    table: HashMap<String<'a>, StringToken>,
    counter: i64,
}

impl<'a> StringPool<'a> {
    pub fn new(bump: &'a Bump) -> Self {
        Self {
            bump,
            table: HashMap::new(),
            counter: 0,
        }
    }

    pub fn intern<'b>(&mut self, s: &'b str) -> StringToken {
        match self.table.entry(String::from_str_in(s, self.bump)) {
            Entry::Occupied(o) => *o.get(),
            Entry::Vacant(v) => {
                let id = self.counter;
                self.counter = id + 1;
                *v.insert(StringToken(id))
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct StringToken(i64);
