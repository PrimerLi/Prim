import scala.collection.mutable.ArrayBuffer
import java.io._
import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.mutable.Set

class Edge
{
    var a: Char = '0'
    var b: Char = '0'
    var weight = 0
    def this(a: Char, b: Char, weight: Int)
    {
        this()
        this.a = a
        this.b = b
        this.weight = weight
    }
    override def toString(): String = 
    {
        return a.toString + " --- " + b.toString + ", weight = " + weight.toString
    }
}

class Prim
{
    var vertices: ArrayBuffer[Char] = new ArrayBuffer[Char]()
    var edges: ArrayBuffer[Edge] = new ArrayBuffer[Edge]()
    var map = Map[Char, ArrayBuffer[Char]]()
    var tree: ArrayBuffer[Edge] = new ArrayBuffer[Edge]()

    def this(fileName: String)
    {
        this()
        val inputFile: File = new File(fileName)
        if (!inputFile.exists())
        {
            println(fileName + " does not exist. ")
            System.exit(-1)
        }

        val source = Source.fromFile(inputFile)
        for (l <- source.getLines)
        {
            val line = l.replace(":", "").replace(",", "")
            val array = line.split(" ")
            val numberOfAdjacentVertices = (array.length - 1)/2
            vertices.append(array(0)(0))
            var adj = new ArrayBuffer[Char]()
            for (i <- 0 to numberOfAdjacentVertices - 1)
            {
                adj.append(array(i + 1)(0))
                val weight = array(i + 1 + numberOfAdjacentVertices).toInt
                val edge = new Edge(array(0)(0), array(i+1)(0), weight)
                edges.append(edge)
            }
            map(array(0)(0)) = adj
        }
        source.close()
    }

    def getEdge(start: Char, end: Char): Edge = 
    {
        for (edge <- edges)
        {
            if (edge.a == start && edge.b == end) return edge
            if (edge.a == end && edge.b == start) return edge
        }
        return new Edge()
    }
    def search(source: Char)
    {
        assert(vertices.contains(source))
        var inside = new ArrayBuffer[Char]()
        var outside = new ArrayBuffer[Char]()
        inside.append(source)
        for (v <- vertices)
        {
            if (!inside.contains(v)) outside.append(v)
        }
        var count = 0
        while(outside.size > 0 && count <= edges.length/2)
        {
            count = count + 1
            var cuts = new ArrayBuffer[Edge]()
            for (v <- inside)
            {
                val out_adj = map(v).filter(outside.contains(_))
                for (i <- 0 to out_adj.length - 1)
                {
                    val start = v
                    val end = out_adj(i)
                    cuts.append(getEdge(start, end))
                }
            }
            var minWeight = cuts(0).weight
            var minIndex = 0
            for (i <- 0 to cuts.length - 1)
            {
                if (minWeight > cuts(i).weight)
                {
                    minWeight = cuts(i).weight
                    minIndex = i
                }
            }
            val minEdge = cuts(minIndex)
            tree.append(minEdge)
            if (inside.contains(minEdge.a))
            {
                inside.append(minEdge.b)
                outside -= minEdge.b
            }
            else
            {
                inside.append(minEdge.a)
                outside -= minEdge.a
            }
        }

        var treeVertices = Set[Char]()
        for (edge <- tree)
        {
            treeVertices += edge.a
            treeVertices += edge.b
        }

        assert(treeVertices.size == vertices.length)
    }
}

object Prim
{
    def main(args: Array[String]): Unit = 
    {
        val prim = new Prim("graph.txt")
        prim.search(prim.vertices(0))
        for (t <- prim.tree)
        {
            println(t)
        }
    }
}
