pub fn map_in_place<T, F>(mut xs: Vec<T>, fun: F) -> Vec<T> 
  where F: FnMut(T) -> T 
{
  // TODO: it is wasteful to copy the elements instead of mutating them in place
  xs.drain(..).map(fun).collect()
}
