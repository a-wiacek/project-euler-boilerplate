// Find-union data structure. Vertices are indexed from 0.
// Nodes keep information about size (number of nodes in connected component).

#[derive(Clone)]
struct Node {
    parent: Option<usize>,
    size: usize,
}

impl Node {
    fn new() -> Node {
        Node {
            parent: None,
            size: 1,
        }
    }
}

pub struct FindUnion {
    nodes: Vec<Node>,
}

impl FindUnion {
    pub fn new(size: usize) -> FindUnion {
        FindUnion {
            nodes: vec![Node::new(); size],
        }
    }

    pub fn find(&mut self, x: usize) -> usize {
        match self.nodes[x].parent {
            None => x,
            Some(parent) => {
                let root = self.find(parent);
                self.nodes[x].parent = Some(root);
                self.nodes[x].size = self.nodes[root].size;
                root
            }
        }
    }

    pub fn union(&mut self, x: usize, y: usize) {
        let root_x = self.find(x);
        let root_y = self.find(y);
        if root_x != root_y {
            if self.nodes[root_x].size > self.nodes[root_y].size {
                self.nodes[root_y].parent = Some(root_x);
                self.nodes[root_x].size += self.nodes[root_y].size;
            } else {
                self.nodes[root_x].parent = Some(root_y);
                self.nodes[root_y].size += self.nodes[root_x].size;
            }
        }
    }

    pub fn size(&mut self, x: usize) -> usize {
        let root = self.find(x);
        self.nodes[root].size
    }
}
