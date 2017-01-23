class Endoflist
{
    def findEnd(list:List[Int],index: Int,prev: Int): (Int,Int) = {
        if(list.isEmpty) (prev,index-1) else findEnd(list.tail,index+1,list.head)
    }
}

object End {
    def main(args: Array[String]) {
    val myobj=new Endoflist
    val list=List(9,8,7,6,5,4,3,2,1)
    val (element,index)=(myobj.findEnd(list,0,0))
    println(s"Last value $element And its index $index")
   
    }
}
