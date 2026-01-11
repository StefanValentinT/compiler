pub trait IsQueue<T: Clone> {
    fn add(&mut self, val: T);
    fn remove(&mut self) -> Result<T, &str>;
    fn peek(&self) -> Result<T, &str>;
    fn size(&self) -> usize;
    fn is_empty(&self) -> bool {
        self.size() <= 0
    }
    fn consume(&mut self) {
        let _ = self.remove();
    }
}

#[derive(Debug)]
pub struct Queue<T: Clone> {
    queue: Vec<T>,
}
impl<T: Clone + PartialEq> Queue<T> {
    pub fn is_there(&self, offset: usize, val: T) -> bool {
        self.queue.get(offset).map(|c| *c == val).unwrap_or(false)
    }
}

impl<T: Clone> Queue<T> {
    pub fn new() -> Queue<T> {
        Queue { queue: vec![] }
    }
}

impl<T: Clone> Default for Queue<T> {
    fn default() -> Queue<T> {
        Queue { queue: vec![] }
    }
}

impl<T: Clone> IsQueue<T> for Queue<T> {
    fn add(&mut self, val: T) {
        self.queue.push(val);
    }

    fn remove(&mut self) -> Result<T, &str> {
        if self.queue.len() > 0 {
            Ok(self.queue.remove(0usize))
        } else {
            Err("The queue is empty")
        }
    }

    fn peek(&self) -> Result<T, &str> {
        match self.queue.first() {
            Some(val) => Ok(val.clone()),
            None => Err("The Queue is empty"),
        }
    }
    fn size(&self) -> usize {
        self.queue.len()
    }
}

pub fn to_queue<T, I>(iter: I) -> Queue<T>
where
    I: IntoIterator<Item = T>,
    T: Clone,
{
    let mut q = Queue::new();
    for item in iter {
        q.add(item);
    }
    q
}

pub fn str_to_queue<S>(s: S) -> Queue<char>
where
    S: AsRef<str>,
{
    let mut q = Queue::new();
    for c in s.as_ref().chars() {
        q.add(c);
    }
    q
}

trait QueueExt {
    fn starts_with(&self, s: &str) -> bool;
}

impl QueueExt for Queue<char> {
    fn starts_with(&self, s: &str) -> bool {
        s.chars().enumerate().all(|(i, c)| self.is_there(i, c))
    }
}
