
case class Student(id: Long, name: String) 
case class Marks(subjectId: Int,studentId: Long, marksObtained: Float) 

class AllModules
{
    /**
  1)
Input:- (subjectId, percentage, pass/fail)
Output:- for input pass, evaluate that how much students(id, name) are passed in the inputted subjectId
	for input fail, evaluate that how much students(id, name) are failed in the inputted subjectId
Note:- percentage is the input which defines the minimum passing criteria
e.g.
Pass count: 15
Fail count: 10
    */
  def findPassFail(markslst: List[Marks],id: Int, percent: Float,choice: String): Int =
     {
         choice match {
             case "pass" => {
             val count= markslst.flatMap(x => if(x.subjectId == id && x.marksObtained > percent) Some(x) else None )
                 count.length
               }
               case "fail" => {
                   val count=markslst.flatMap(x => if(x.subjectId == id && x.marksObtained < percent) Some(x) else None)
               
                   count.length
               }
               
         }
     }
/**
    * 2)
    * Input:- (subjectId, count, top/bottom)
    * Output:- based on the last input(top/bottom), output the students details who have scored max/min in that subjectId
    *e.g.
    * input: 1 5 top
    * output:
    * Kunal 85
    * Himanshu 84
    * Geetika 83
    * Anmol 82
    * Mahesh 81
    */
     
     def findTopBottom(mrksList: List[Marks],id: Int,count:Int,choice: String): List[Float] =
{
    choice match {
        
     case "top" => { 
         val toppers=mrksList.flatMap(x => if(x.subjectId == id) Some(x.marksObtained) else None)
         toppers.sorted.drop(toppers.length-count).reverse
         
     } 
     case "bottom" => {
         val bottoms=mrksList.flatMap(x => if(x.subjectId == id) Some(x.marksObtained) else None)
         bottoms.sorted.reverse.drop(bottoms.length-count).reverse
     }
    }
}


  /**
  3)
Input:-
(top/bottom, count)
OutPut:-
Overall top/least scorer based on all the subjects score, fetch students name
count- input defines that how much students name are to be printed on console
e.g.
input: top 2
output:
Himanshu 75%
Geetika 74%
    */

     
     
     
}

object StudentDetails
{
    def main(args: Array[String]) {
    val myobj=new AllModules
    val studentlist: List[Student] = List(Student(1, "Kunal"), Student(2, "Himanshu"), Student(3, "Shivangi"), Student(4, "Mahesh"), Student(5, "Akhil"),Student(6, "Nitin"),Student(7, "Prakash"), Student(8, "Mansi"), Student(9, "Naveen"), Student(10, "Manoj"))
    val marksList: List[Marks]= List( Marks(1, 1, 100),Marks(1, 2, 100), Marks(1, 3, 67),Marks(1, 4, 70),Marks(1, 5, 80),
                          Marks(1, 6, 40), Marks(1, 7, 76),Marks(1, 8, 70),Marks(1, 9, 96),Marks(1, 10, 56),
                          Marks(2, 1, 90),Marks(2, 2, 100),Marks(2, 3, 90),Marks(2, 4, 95),Marks(2, 5, 60),
                          Marks(2, 6, 80), Marks(2, 7, 66),Marks(2, 8, 80),Marks(2, 9, 76),Marks(2, 10, 86),
                          Marks(3, 1, 85),Marks(3, 2, 80),Marks(3, 3, 80),Marks(3, 4, 80),Marks(3, 5, 80),
                          Marks(3, 6, 80),Marks(3, 7, 80),Marks(3, 8, 80),Marks(3, 9, 80),Marks(3, 10, 80),
                          Marks(4, 1, 60),Marks(4, 2, 60),Marks(4, 3, 60),Marks(4, 4, 60),Marks(4, 5, 60),
                          Marks(4, 6, 60),Marks(4, 7, 70),Marks(4, 8, 60),Marks(4, 9, 60),Marks(4, 10, 60),
                          Marks(5, 1, 90),Marks(5, 2, 85),Marks(5, 3, 80),Marks(5, 4, 90),Marks(5, 5, 95),
                          Marks(5, 6, 50),Marks(5, 7, 90),Marks(5, 8, 70),Marks(5, 9, 74),Marks(5, 10, 60)
                           )
     println(myobj.findPassFail(marksList,1,33,"pass"))
     
      println(myobj.findTopBottom(marksList,1,2,"top"))
     // println(myobj.overAllTopBottom(marksList,studentslist,2, "Top").map(x=> println(s"${x._1} : ${x._2}%")))
     
    }
}
