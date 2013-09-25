package scrabblesolver
import scala.tools.jline
import scala.collection.mutable.{ Set, Map }
import java.io.File
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import java.io.FileInputStream

/*On evening I remember me thinking I possibly do business with white  kerchiefs
 * 
 */

object scrabblesolver extends App {
    type Key = (String, Int)
    lazy val map1: Map[Key, Set[String]] = load();

    //    lazy val map = scala.collection.immutable.Map(1->2)

    def string2AllKeys(s: Array[String]): List[Key] =
        string2Keys(s).
            map({ case (x, y) => (1 to y).map((x, _)) }).
            flatten

    def string2Keys(s: Array[String]): List[Key] =
        s.groupBy(x => x).toList.
            map({ case (x, y) => (x, y.length) })

    def collect(si: Iterator[String]): Unit = {
        for {
            s <- si
            key <- string2Keys(s.split("").tail)
        } {
            //            println(map)
            if (map1 contains key) {
                map1(key) += s
            } else {
                map1(key) = Set[String](s)
            }
        }
        save(map1);
    }

    def save(map: Map[Key, Set[String]]): Unit = {
        val out = new ObjectOutputStream(
            new FileOutputStream(new File("scrabble.dat")));
        out.writeObject(map);
        out.close();
    }
    def load(): Map[Key, Set[String]] = {
        try {
            new ObjectInputStream(
                new FileInputStream(new File("scrabble.dat"))).readObject().
                asInstanceOf[Map[Key, Set[String]]];
        } catch {
            case t: Throwable =>
                println("Can't read a base becouse:" + t);
                Map[Key, Set[String]]();
        }
    }

    def merge(weighted: List[(String, Int)], addition: List[String],
              outcome: List[(String, Int)]): List[(String, Int)] =
        (weighted, addition) match {
            case ((w, n) :: weighted, a :: addition) => if (w > a)
                merge((w, n) :: weighted, addition, (a, 1) :: outcome)
            else if (w < a)
                merge(weighted, a :: addition, (w, n) :: outcome)
            else
                merge(weighted, addition, (w, n + 1) :: outcome)
            case ((w, n) :: weighted, List()) => merge(weighted, List(), (w, n) :: outcome)

            case (List(), a :: addition)      => merge(List(), addition, (a, 1) :: outcome)
            case _                            => outcome.reverse
        }

    def group(ss: List[String], letters: String) = {
        val alphabet = letters.split("").toSet;
        ss.map(s => {
            s.split("").
                filter(c => !(alphabet contains c)).
                sorted.mkString -> s
        }).groupBy(_._1).map({
            case (key, values) => key -> values.map(_._2)
        }).toList.sortBy(_._1)
    }

    def lookup(letters: String) = {
        string2Keys(letters.split("").tail).map(map1).
            foldLeft(List[(String, Int)]())({
                case (outcome: List[(String, Int)], x) => merge(outcome, x.toList.sorted, List())
            }).
            filter({ case (w, n) => w.length >= n - 1 && w.length <= n + 1 }).
            map(_._1)
    }

    override def main(args: Array[String]): Unit = {
        //map1 = Map[Key, Set[String]]()
        if (args.size > 0) {
            collect(
                args.toIterator.map(x => io.Source.fromFile(x).getLines).
                    flatten.map(
                        x => """\W+""".r.split(x).
                            map(_.trim).filter(_.length > 1)).flatten)
            save(map1);
        }

        val ConsoleReader = new jline.console.ConsoleReader()

        var groupped: Boolean = true;
        while (true) {
            ConsoleReader.readLine("> ").split(" ").map(_.trim).toList match {
                case "group" :: xs => groupped = true;
                case "plain" :: xs => groupped = false;

                case xs => try {
                    if (groupped) {
                        println("group");
                        for (w <- xs) {
                            println("== %s ==".format(w))
                            for ( (key, advices) <- group(lookup(w), w) ) {
                                print(" %s => ".format(key))
                                for (advice <- advices.sortBy(-_.length)) {
                                    print("%s ".format(advice));
                                }
                                println("")
                            }
                        }
                    } else {
                        for (w <- xs) {
                            println("Plain output")
                            println("== %s ==".format(w))
                            val advices = lookup(w)
                            for (advice <- advices) {
                                println("%s".format(advice))
                            }
                            println("Count: %s".format(advices.length))
                        }
                    }
                } catch {
                    case x => println("Command failure: " + x)
                }
            }
        }
    }
}