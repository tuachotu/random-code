/*
Given n non-negative integers a1, a2, ..., an ,
where each represents a point at coordinate (i, ai).
n vertical lines are drawn such that the two endpoints of line i is at (i, ai) and (i, 0).
Find two lines, which together with x-axis forms a container, such that the container contains the most water.
Input: [1,8,6,2,5,4,8,3,7]
output 49
Note: You may not slant the container and n is at least 2.
 */
object ContainerWithMostWater extends  App {

  def maxArea1(height: Array[Int]): Int = {
    val areas = for {
      i <- height.indices
      j <- i + 1  until height.length
      area = Math.min(height(i), height(j)) * (j-i )
    } yield area
    areas.max
  }

  def maxArea(height: Array[Int]): Int = {
    def getArea(l: Int, r: Int): Int = Math.min(height(l), height(r))*(r-l)
    var left = 0
    var right = height.length
    var area = 0
    while (right > left) {
      area = Math.max(area, getArea(left, right))

    }
    val areas = for {
      i <- height.indices
      j <- i + 1  until height.length
      area = Math.min(height(i), height(j)) * (j-i )
    } yield area
    areas.max
  }



  println(maxArea(Array(1,8,6,2,5,4,8,3,7)))
  println(maxArea(Array(1,1)))
}
