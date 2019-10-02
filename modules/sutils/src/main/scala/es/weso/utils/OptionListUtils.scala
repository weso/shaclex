package es.weso.utils

object OptionListUtils {

 def maybeAddList[A](maybeList: Option[List[A]],
                     other: List[A]
                    ): Option[List[A]] =
   if (other.isEmpty) maybeList
   else maybeList match {
     case None => Some(other)
     case Some(ls) => Some(ls ++ other)
   }

}