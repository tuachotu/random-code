object ValidateIP extends App {


   def validateIPv4(s: String): Boolean = {
     val allowedChars: String = "0123456789"
     def isNumber(s: String): Boolean = s.foldLeft(true) {(res, c) => if (res) allowedChars contains c else res}
     s.split("\\.").filter(_.nonEmpty).toList match {
       case Nil => false
       case l if l.length != 4 => false
       case l if s.count(_ == '.') != 3 => false
       case l => l.foldLeft(true) { (status, number) =>
         println(status, number)
         if (isNumber(number) && 0 <= number.toInt && 255 >= number.toInt && (number.length == 1 || number.head != '0')) {
           println("why")
         }
         status match {
           case false => false
           case true => isNumber(number) && 0 <= number.toInt && 255 >= number.toInt && (number.length == 1 || number.head != 0)
         }
       }
     }
   }

  def validateIPv6(s: String): Boolean = {
    val allowedChars: String = "0123456789ABCDEFabcdef"
    s.split(":").filter(_.nonEmpty).toList match {
      case Nil => false
      case l if s.count(_ == ':') != 7 => false
      case l if l.length != 8 => false
      case l => l.foldLeft(true) { (status, number) =>
        status match {
          case false => false
          case true =>
            if (number.length > 4) false
            else number.foldLeft(true) { (valid, symbol) =>
              valid match {
                case false => false
                case true => allowedChars contains symbol
              }
            }
        }
      }
    }
  }

  def validIPAddress(IP: String): String = {
    if (IP.isEmpty || IP.length > 60) "Neither"
    else if (validateIPv4(IP)) "IPv4"
    else if (validateIPv6(IP)) "IPv6"
    else "Neither"
  }

  //"2001:0db8:85a3:0:0:8A2E:0370:7334:"
  //println(validIPAddress("1e1.4.5.6"))
  //println(validIPAddress("172.16.254.1"))
  println(validIPAddress("01.01.01.01"))
 /* println(validIPAddress("2001:0db8:85a3:0000:0000:8a2e:0370:7334"))
  println(validIPAddress("2001:db8:85a3:0:0:8A2E:0370:7334"))
  println(validIPAddress("2001:0db8:85a3::8A2E:0370:7334")) // invalid
  println(validIPAddress("02001:0db8:85a3:0000:0000:8a2e:0370:7334")) // invalid
*/
}
