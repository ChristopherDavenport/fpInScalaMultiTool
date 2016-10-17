package fpinscalamulti.Vanilla

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
/**
  * Created by davenpcm on 10/16/16.
  */
package object parallelism {

  type Par[A] = ExecutorService => Future[A]
}
