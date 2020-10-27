object Aggregate extends App {
  def myMin(p: Double, q: Double, r: Double): Double ={
    if(p<=q)
      if(p<=r)
        return p
      else return r
    else if(q<=r)
      return q
    else return r
  }
  def myMean(p: Double, q: Double, r: Double): Double ={
   return (p+q+r)/3
  }
  def myMed(p: Double, q: Double, r: Double): Double ={
    if (p<q)
      if (q<r)
        return q
      else
        return r
    else if (p>r)
          return r
          else
          return p
  }
}

