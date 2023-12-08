using System.Text;

namespace AdventOfCode2023.Day8;

public static class Day8
{
    public static int Part1(IEnumerable<string> input)
    {
        var leftRights = input.First().ToCharArray();
        var graph = GetGraph(input);

        var currentVertex = "AAA";
        var vertices = graph[currentVertex];
        var leftRightsIndex = 0;
        var counter = 0;

        do
        {
            counter++;
            if (leftRightsIndex >= leftRights.Length)
            {
                leftRightsIndex = 0;
            }
            var leftRight = leftRights.ElementAt(leftRightsIndex);
            var (left, right) = vertices;
            var nextVertex = leftRight == 'L' ? left : right;
            currentVertex = nextVertex;
            vertices = graph[currentVertex];
            leftRightsIndex++;
        } while (currentVertex != "ZZZ");

        return counter;
    }

    public static long Part2(IEnumerable<string> input)
    {
        var leftRights = input.First().ToCharArray();
        var graph = GetGraph(input);

        var currentVertices = graph.Where(vertex => vertex.Key.EndsWith('A')).Select(vertex => vertex.Key);
        var leftRightsIndex = 0;
        var counter = 0;
        var firstEntries = new List<int>();
        
        do
        {
            counter++;
            if (leftRightsIndex >= leftRights.Length)
            {
                leftRightsIndex = 0;
            }
            var leftRight = leftRights.ElementAt(leftRightsIndex);
            
            var nextVertices = currentVertices.Select(vertex =>
            {
                var vertices = graph[vertex];
                var (left, right) = vertices;
                var nextVertex = leftRight == 'L' ? left : right;
                return nextVertex;
            });
            
            currentVertices = nextVertices;
            leftRightsIndex++;
            
            currentVertices.ToList().ForEach(v =>
            {
                if (v.EndsWith('Z'))
                {
                    firstEntries.Add(counter);
                }
            });
            
        } while ((firstEntries.Count != currentVertices.Count()));
        
        return NWW(firstEntries.Select(n => (long) n));
    }
    
    private static Dictionary<string, (string, string)> GetGraph(IEnumerable<string> input)
    {
        return input.Skip(2).Aggregate(new Dictionary<string, (string, string)>(), (acc, line) =>
        {
            var parts = line.Split(" = ");
            var name = parts.First();
            var valuesPart = parts.Last();
            var valuesLine = valuesPart.Skip(1).Take(valuesPart.Length - 2).ToList().Aggregate("", (s, c) => s + c);
            var values = valuesLine.Split(", ");
            
            acc.Add(name, (values.First(), values.Last()));
            return acc;
        });
    }

    private static long NWD(long a, long b)
    {
        while (a != b)
        {
            if (a > b)
            {
                a -= b;
            }
            else
            {
                b -= a;
            }
        }

        return a;
    }

    private static long NWW(long a, long b)
    {
        return a * b / NWD(a, b);
    }

    private static long NWW(IEnumerable<long> numbers)
    {
        return numbers.Aggregate((acc, n) => NWW(acc, n));
    }
}