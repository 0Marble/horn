use std::{
    collections::{LinkedList, VecDeque},
    fmt::Display,
    hash::Hash,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SetItem(usize);

#[derive(Default, Debug, Clone)]
pub struct DisjointSet<T> {
    classes: Vec<(LinkedList<SetItem>, usize)>,
    sets: Vec<usize>,
    items: Vec<T>,
    vacant: VecDeque<usize>,
}

impl<T> Display for DisjointSet<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for repr in self.set_representatives() {
            write!(f, "{repr:?}\n\t")?;
            for (s, item) in self.set_of(repr).map(|s| (s, self.get(s).unwrap())) {
                write!(f, "{item}[{}], ", self.sets[s.0])?;
            }
            writeln!(f)?;
        }

        Ok(())
    }
}

impl<T> DisjointSet<T> {
    pub fn add_set(&mut self, val: T) -> SetItem {
        let set_idx = if let Some(idx) = self.vacant.pop_front() {
            idx
        } else {
            self.classes.push(Default::default());
            self.classes.len() - 1
        };
        self.sets.push(set_idx);
        self.items.push(val);
        let new_item = SetItem(self.items.len() - 1);
        self.classes[set_idx] = (LinkedList::from([new_item]), 1);
        new_item
    }

    pub fn find(&self, item: SetItem) -> Option<SetItem> {
        self.classes
            .get(self.sets[item.0])
            .map(|set| set.0.front().cloned().unwrap())
    }

    pub fn get(&self, item: SetItem) -> Option<&T> {
        self.items.get(item.0)
    }

    pub fn merge<'a>(&'a mut self, a: SetItem, b: SetItem, rule: impl Fn(&'a T, &'a T) -> u8) {
        assert!(self.sets[a.0] < self.classes.len());
        assert!(self.sets[b.0] < self.classes.len());
        assert!(!self.vacant.contains(&self.sets[a.0]));
        assert!(!self.vacant.contains(&self.sets[b.0]));
        assert!(a.0 < self.items.len());
        assert!(b.0 < self.items.len());

        let s = self.find(a).unwrap();
        let t = self.find(b).unwrap();
        if s == t {
            return;
        }
        let s_item = &self.items[s.0];
        let t_item = &self.items[t.0];
        let repr = rule(s_item, t_item);
        let (s_idx, t_idx) = if repr == 1 {
            (self.sets[s.0], self.sets[t.0])
        } else if repr == 2 {
            (self.sets[t.0], self.sets[a.0])
        } else if self.classes[self.sets[s.0]].1 > self.classes[self.sets[t.0]].1 {
            (self.sets[s.0], self.sets[t.0])
        } else {
            (self.sets[t.0], self.sets[s.0])
        };

        let (mut s_set, s_len) = std::mem::take(&mut self.classes[s_idx]);
        let (mut t_set, t_len) = std::mem::take(&mut self.classes[t_idx]);
        for item in &t_set {
            self.sets[item.0] = s_idx;
        }
        s_set.append(&mut t_set);
        self.classes[s_idx] = (s_set, s_len + t_len);
        self.vacant.push_back(t_idx);
    }

    pub fn no_rule(_a: &T, _b: &T) -> u8 {
        0
    }

    pub fn set_count(&self) -> usize {
        self.classes.len() - self.vacant.len()
    }

    pub fn set_representatives(&self) -> impl Iterator<Item = SetItem> + '_ {
        self.classes.iter().flat_map(|(s, _)| s.front()).cloned()
    }

    pub fn set_of(&self, i: SetItem) -> impl Iterator<Item = SetItem> + '_ {
        self.classes[i.0].0.iter().cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn merge_all() {
        let mut set = DisjointSet::default();
        let mut items = vec![];
        for i in 0..20 {
            items.push(set.add_set(i));
        }

        let mut w_size = 1;
        while w_size < items.len() {
            for w in items.chunks(w_size + 1) {
                set.merge(
                    w.first().unwrap().clone(),
                    w.last().unwrap().clone(),
                    DisjointSet::no_rule,
                );
            }
            println!("{set}");
            w_size *= 2;
        }

        assert_eq!(set.set_count(), 1);
    }
}
