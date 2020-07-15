#[derive(Debug, PartialEq)]
pub enum Object {
    HeapString,
}

#[derive(Debug, PartialEq)]
pub struct HeapObject {
    obj_type: Object,
}

#[derive(Debug, PartialEq)]
pub struct HeapString {
    length: usize,
    strings: String,
}

impl HeapString {
    pub fn new(string: &str) -> Self {
        Self {
            length: string.len(),
            strings: String::from(string),
        }
    }
    // Copies the string and adds it to the heap
    // fn transfer_string(&mut self, string: &'_ str) {
    //     self.strings.push(String::from(string))
    // }
}