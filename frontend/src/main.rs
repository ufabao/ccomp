use frontend::front_end_passes;

fn main() {
  let ast = front_end_passes("blah.c");
  dbg!(ast);
}
