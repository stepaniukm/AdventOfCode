namespace AdventOfCode2023.Day10;

internal record Point(char value, int x, int y)
{
    public static char Empty { get; } = '.';
    public static char Start { get; } = 'S';
    public override string ToString()
    {
        return $"({x}, {y}) = {value}";
    }

    public virtual bool Equals(Point? other)
    {
        if (ReferenceEquals(null, other)) return false;
        if (ReferenceEquals(this, other)) return true;
        return value == other.value && x == other.x && y == other.y;
    }
};

public static class Day10
{
    public static int Part1(List<string> lines)
    {
        var map = lines.Select((line, y) => line.ToCharArray().Select(((c, x) => new Point(c, x, y))).ToList()).ToList();
        var startingPoint = map.SelectMany(x => x).First(x => x.value == 'S');
        if (startingPoint == null) throw new Exception("No starting point found!");
        
        var visited = new List<Point>{ startingPoint };
        var startingPointNeighbors = GetConnectibleNeighbors(startingPoint, map, visited);
        
        if (startingPointNeighbors.Count != 2) throw new Exception("Too many or too little connectible neighbours found!");
        var counter = 1;
        var currentPointA = startingPointNeighbors.First();
        var currentPointB = startingPointNeighbors.Last();
        
        visited.Add(currentPointA);
        visited.Add(currentPointB);

        do
        {
            var newCurrentPointA = GetConnectibleNeighbors(currentPointA, map, visited).First();
            var newCurrentPointB = GetConnectibleNeighbors(currentPointB, map, visited).First();
            counter++;
            visited.Add(newCurrentPointA);
            visited.Add(newCurrentPointB);
            currentPointA = newCurrentPointA;
            currentPointB = newCurrentPointB;
            
        } while (currentPointA != currentPointB);

        return counter;
    }
    
    public static int Part2(List<string> lines)
    {
        var map = lines.Select((line, y) => line.ToCharArray().Select(((c, x) => new Point(c, x, y))).ToList()).ToList();
        var startingPoint = map.SelectMany(x => x).First(x => x.value == 'S');
        if (startingPoint == null) throw new Exception("No starting point found!");
        
        var visited = new List<Point>{ startingPoint };
        var oneSide = new List<Point>();
        var otherSide = new List<Point>();
        string insideSide = null;
        
        var startingPointNeighbors = GetConnectibleNeighbors(startingPoint, map, visited);
        if (startingPointNeighbors.Count != 2) throw new Exception("Too many or too little connectible neighbours found!");

        var previousPoint = startingPoint;
        var currentPoint = startingPointNeighbors.Last();
        var (oneSideUnvisitedEmptyNeighbors, otherSideUnvisitedEmptyNeighbors) = GetPointSidesEmptyNeighbors(previousPoint, currentPoint, map, visited);
        
        Console.WriteLine(currentPoint);

        return 0;
    }

    private static (List<Point> oneSideUnvisitedEmptyNeighbors, List<Point> otherSideUnvisitedEmptyNeighbors) GetPointSidesEmptyNeighbors(Point previousPoint, Point currentPoint, List<List<Point>> map, List<Point> visited)
    {
        List<(int x, int y)>? oneSideVectors = null;
        List<(int x, int y)>? otherSideVectors = null;
        if (previousPoint.value == 'S' && currentPoint.value == '7')
        {
            oneSideVectors = [(0, -1), (1, 0)];
            otherSideVectors = [];
        }
        if (previousPoint.value == 'S' && currentPoint.value == 'J')
        {
            oneSideVectors = [];
            otherSideVectors = [(0, 1), (1, 0)];
        }
        if (previousPoint.value == 'S' && currentPoint.value == 'F')
        {
            oneSideVectors = [(-1, 0), (0, -1)];
            otherSideVectors = [];
        }
        if (previousPoint.value == 'S' && currentPoint.value == 'L')
        {
            oneSideVectors = [];
            otherSideVectors = [(0, 1), (-1, 0)];
        }
        if (previousPoint.value == 'S' && currentPoint.value == '|' && currentPoint.y > previousPoint.y)
        {
            oneSideVectors = [(1, 0)];
            otherSideVectors = [(-1, 0)];
        }
        if (previousPoint.value == 'S' && currentPoint.value == '|' && currentPoint.y < previousPoint.y)
        {
            oneSideVectors = [(-1, 0)];
            otherSideVectors = [(1, 0)];
        }
        if (previousPoint.value == 'S' && currentPoint.value == '-' && currentPoint.x > previousPoint.x)
        {
            oneSideVectors = [(0, -1)];
            otherSideVectors = [(0, 1)];
        }
        if (previousPoint.value == 'S' && currentPoint.value == '-' && currentPoint.x < previousPoint.x)
        {
            oneSideVectors = [(0, 1)];
            otherSideVectors = [(0, -1)];
        }
        
        var oneSideUnvisitedEmptyNeighbors = new List<Point>();
        var otherSideUnvisitedEmptyNeighbors = new List<Point>();

        if (oneSideVectors != null)
            foreach (var (x, y) in oneSideVectors)
            {
                var neighborRow = currentPoint.y + y >= map.Count || currentPoint.y + y < 0
                    ? null
                    : map.ElementAt(currentPoint.y + y);
                if (neighborRow == null) continue;
                var neighbor = currentPoint.x + x >= neighborRow.Count || currentPoint.x + x < 0
                    ? null
                    : neighborRow.ElementAt(currentPoint.x + x);
                if (neighbor == null) continue;
                if (visited.Contains(neighbor)) continue;
                if (neighbor.value != Point.Empty) continue;
                oneSideUnvisitedEmptyNeighbors.Add(neighbor);
            }

        if (otherSideVectors == null) return (oneSideUnvisitedEmptyNeighbors, otherSideUnvisitedEmptyNeighbors);
        {
            foreach (var (x, y) in otherSideVectors)
            {
                var neighborRow = currentPoint.y + y >= map.Count || currentPoint.y + y < 0
                    ? null
                    : map.ElementAt(currentPoint.y + y);
                if (neighborRow == null) continue;
                var neighbor = currentPoint.x + x >= neighborRow.Count || currentPoint.x + x < 0
                    ? null
                    : neighborRow.ElementAt(currentPoint.x + x);
                if (neighbor == null) continue;
                if (visited.Contains(neighbor)) continue;
                if (neighbor.value != Point.Empty) continue;
                otherSideUnvisitedEmptyNeighbors.Add(neighbor);
            }
        }

        return (oneSideUnvisitedEmptyNeighbors, otherSideUnvisitedEmptyNeighbors);
    }

    private static List<Point> GetConnectibleNeighbors(Point startingPoint, IReadOnlyList<List<Point>> map, List<Point> visited)
    {
        var neighborVectors = new List<(int x, int y)>
        {
            (0, 1),
            (0, -1),
            (1, 0),
            (-1, 0),
        };
        var connectibleCorners = GetShapeConnectibleCorners(startingPoint.value);
        
        var neighbors = new List<Point>();
        
        foreach (var (x, y) in neighborVectors)
        {
            
            var neighborRow = startingPoint.y + y >= map.Count || startingPoint.y + y < 0 ? null : map.ElementAt(startingPoint.y + y);
            if (neighborRow == null) continue;
            var neighbor = startingPoint.x + x >= neighborRow.Count || startingPoint.x + x < 0 ? null : neighborRow.ElementAt(startingPoint.x + x);
            if (neighbor == null) continue;
            if (visited.Contains(neighbor)) continue;
            if (neighbor.value == Point.Empty) continue;
            if (!connectibleCorners.ContainsKey((x, y))) continue;

            if (connectibleCorners[(x, y)].Contains(neighbor.value)) neighbors.Add(neighbor);
        }
        
        return neighbors;
    }

    private static Dictionary<(int x, int y), List<char>> GetShapeConnectibleCorners(char startingPointValue)
    {
        // pipes | - F L J 7 S
        
        // starting point S
        if (startingPointValue == Point.Start)
        {
            return new Dictionary<(int x, int y), List<char>>
            {
                { (0, -1), ['F', '|', '7'] },
                { (0, 1), ['L', '|', 'J'] },
                { (1, 0), ['J', '-', '7'] },
                { (-1, 0), ['F', '-', 'L'] },
            };
        }

        return startingPointValue switch
        {
            'F' => new Dictionary<(int x, int y), List<char>>
            {
                { (0, 1), ['L', '|', 'J'] }, { (1, 0), ['J', '-', '7'] },
            },
            '|' => new Dictionary<(int x, int y), List<char>>
            {
                { (0, -1), ['F', '|', '7'] }, { (0, 1), ['L', '|', 'J'] },
            },
            '7' => new Dictionary<(int x, int y), List<char>>
            {
                { (0, 1), ['L', '|', 'J'] }, { (-1, 0), ['F', '-', 'L'] },
            },
            'J' => new Dictionary<(int x, int y), List<char>>
            {
                { (0, -1), ['F', '|', '7'] }, { (-1, 0), ['F', '-', 'L'] },
            },
            '-' => new Dictionary<(int x, int y), List<char>>
            {
                { (1, 0), ['J', '-', '7'] }, { (-1, 0), ['F', '-', 'L'] },
            },
            'L' => new Dictionary<(int x, int y), List<char>>
            {
                { (0, -1), ['F', '|', '7'] }, { (1, 0), ['J', '-', '7'] },
            },
            _ => throw new Exception("Incorrect starting point value!")
        };
    }
}