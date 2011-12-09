package shoot

import unfiltered.request._
import unfiltered.response.{Stream => _, _}
import unfiltered.netty.cycle.Planify

/* http://github.com/teigen/shoot */
object Shoot extends App {

  /* sudoku solver */
  def solve(a:Seq[Int]):Stream[Seq[Int]] = a.indexOf(0) match {
    case -1 => Stream(a)
    case i =>
      val excluded = for {
        j <- 0 until 81
        if i/9==j/9 || (i-j)%9==0 || (i/27==j/27 && i%9/3==j%9/3) //same row || column || block
      } yield a(j)

      for {
        m <- (1 to 9).toStream if !excluded.contains(m)
        res <- solve(a.take(i) ++ Seq(m) ++ a.drop(i+1))
      } yield res
    }

  /* http://unfiltered.databinder.net/Unfiltered.html */
  val sudoku = Planify {

    case GET(Path("/")) =>
      val board = Seq.fill(81)(0)
      render(board)

    case POST(Params(params) & Path("/")) =>
      val board = (0 until 81).map{ n =>
        try{ params(n.toString).head.toInt } catch { case _ => 0 } }
      solve(board).headOption match {
        case Some(solved) => render(solved)
        case None => render(board, Some("#fail"))
      }

    case GET(Path("/styles.css")) => css
  }

  /* run webapp using netty */
  unfiltered.netty.Http(8080).plan(sudoku).run()

  /* html */
  def render(board:Seq[Int], msg:Option[String] = None) = {
    val table = board.zipWithIndex.grouped(9)

    Html(<html>
    <head>
      <title>Shoot!</title>
      <link rel="stylesheet" href="/styles.css" type="text/css" />
    </head>
    <body>
      {msg.toSeq.flatMap(m => <h1>{m}</h1>)}
      <div id="outer">
      <form action="/" method="POST">
        <table id="board">{ table.flatMap{ row =>
        <tr>
          {row.flatMap{ case (column, name) =>
            <td><input type="text" value={if (column == 0) "" else column.toString} size="1" maxlength="1" name={name.toString}/></td>
          }}
        </tr>}
      } <tr><td colspan="9"><input type="submit" value="solve" name="solve"/></td></tr>
        </table>
      </form>
      </div>
    </body>
  </html>)}

  /* css */
  def css = CssContent ~> ResponseString("""
body, html {
    background: #333;
    margin: 0;
    height: 100%;
}

#outer {
    display: table;
    height: 100%;
    width: 100%;
    overflow: hidden;
}

form {
    display: table-cell;
    vertical-align: middle;
    width: 100%;
}

#board {
    margin-left: auto;
    margin-right: auto;
}

input[type=text] {
    width: 2em;
    height: 1.5em;
    font-size: 2em;
    text-align: center;
    border: 3px inset #ccc;
    border-radius: 0.2em;
}

td:nth-child(3n) {
    padding-right: 0.8em;
}

td:last-child {
    padding-right: inherit;
}

tr:nth-child(3n) td {
    padding-bottom: 0.8em;
}

input[type=submit] {
    width: 100%;
    font-size: 2em;
    border: 3px outset #ccf;
    border-radius: 0.2em;
    background: #f0f9ff; /* Old browsers */
    background: -moz-linear-gradient(top,  #f0f9ff 0%, #cbebff 47%, #a1dbff 100%); /* FF3.6+ */
    background: -webkit-gradient(linear, left top, left bottom, color-stop(0%,#f0f9ff), color-stop(47%,#cbebff), color-stop(100%,#a1dbff)); /* Chrome,Safari4+ */
    background: -webkit-linear-gradient(top,  #f0f9ff 0%,#cbebff 47%,#a1dbff 100%); /* Chrome10+,Safari5.1+ */
    background: -o-linear-gradient(top,  #f0f9ff 0%,#cbebff 47%,#a1dbff 100%); /* Opera 11.10+ */
    background: -ms-linear-gradient(top,  #f0f9ff 0%,#cbebff 47%,#a1dbff 100%); /* IE10+ */
    background: linear-gradient(top,  #f0f9ff 0%,#cbebff 47%,#a1dbff 100%); /* W3C */
    filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#f0f9ff', endColorstr='#a1dbff',GradientType=0 ); /* IE6-9 */
}

input[type=submit]:active {
    border: 3px inset #ccf;
    background: #a1dbff; /* Old browsers */
    background: -moz-linear-gradient(top,  #a1dbff 0%, #cbebff 53%, #f0f9ff 100%); /* FF3.6+ */
    background: -webkit-gradient(linear, left top, left bottom, color-stop(0%,#a1dbff), color-stop(53%,#cbebff), color-stop(100%,#f0f9ff)); /* Chrome,Safari4+ */
    background: -webkit-linear-gradient(top,  #a1dbff 0%,#cbebff 53%,#f0f9ff 100%); /* Chrome10+,Safari5.1+ */
    background: -o-linear-gradient(top,  #a1dbff 0%,#cbebff 53%,#f0f9ff 100%); /* Opera 11.10+ */
    background: -ms-linear-gradient(top,  #a1dbff 0%,#cbebff 53%,#f0f9ff 100%); /* IE10+ */
    background: linear-gradient(top,  #a1dbff 0%,#cbebff 53%,#f0f9ff 100%); /* W3C */
    filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#a1dbff', endColorstr='#f0f9ff',GradientType=0 ); /* IE6-9 */
}
  """)
}