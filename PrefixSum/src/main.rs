fn main() {
    println!("Hello, world!");
}
struct Node {
    left : Option<Box<Node>>,
    original : u64,
    right : Option<Box<Node>>
}
fn parallel_prefix_sum(xs: &Vec<u64>) -> Vec<u64> {
    fn up(v: &Vec<u64>) -> Node {
        if v.len() == 0 {
            return Node {left: None, original: 0, right: None};
        }
        if v.len() == 1 {
            return Node {left: None, original: v[0], right: None};
        }
        let (left, right) = v.split_at(v.len()/2);
        let (leftNode, rightNode) = join(|| up(&left.clone().to_vec()), || up(&right.clone().to_vec()));
        let lv = leftNode.original.clone();
        let rv = rightNode.original.clone();
        Node{left: Some(Box::new(leftNode)), original: lv+rv, right: Some(Box::new(rightNode)) }
    }
    fn createVector(p: &Option<Box<Node>>, temp: u64) -> Vec<u64> {
        if p.is_none() {
            vec!(0)
        }
        else {
            let node = p.as_ref().unwrap();
            if node.left.is_some() && node.right.is_some() {
                let (left_vec, right_vec) = join(|| createVector(&node.left, temp - node.right.as_ref().unwrap().original), || createVector(&node.right, temp));concatinate(&left_vec, &right_vec)
            }else {
                vec!(temp)
            }
        }
    }
    let tree = up(&xs.clone());
    let max = tree.original.clone();
    let pps = createVector(&Some(Box::new(tree)), max);
    if (xs.len()<1) {
        pps
    }else {
        concatinate(&*vec![0u64], &*pps)
    }
}
fn concatinate(a: &[u64], b: &[u64]) -> Vec<u64> {
    [a, b].concat()
}