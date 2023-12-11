namespace AdventOfCode2023.Day11;

internal enum PointValue
{
    Galaxy = '#',
    Empty = '.'
}

internal interface IShape
{
};
internal record Point(long X, long Y, PointValue Value): IShape;
internal record HorizontalGap(long X, long width): IShape;

internal record VerticalGap(long Y, long height): IShape;
public static class Day11
{
    public static long Part1(List<string> lines)
    {
        var map = ParseLines(lines);
        var expandedMap = ExpandMap(map);
        var galaxies = expandedMap.SelectMany(line => line).Where(point => point is Point { Value: PointValue.Galaxy }).Select(p => (Point) p).ToList();
        
        long distances = 0;
        foreach (var galaxyA in galaxies)
        {
            foreach (var galaxyB in galaxies)
            {
                var distance = Math.Abs(galaxyA.X - galaxyB.X) + Math.Abs(galaxyA.Y - galaxyB.Y);
                distances += distance;
            }
        }

        return distances / 2;
    }

    public static long Part2(List<string> lines)
    {
        var map = ParseLines(lines);
        var expandedMap = ExpandMap2(map);
        var galaxies = expandedMap.SelectMany(line => line).Where(point => point is Point { Value: PointValue.Galaxy }).Select(p => (Point) p).ToList();
        
        long distances = 0;
        foreach (var galaxyA in galaxies)
        {
            foreach (var galaxyB in galaxies)
            {
                var distance = Math.Abs(galaxyA.X - galaxyB.X) + Math.Abs(galaxyA.Y - galaxyB.Y);
                distances += distance;
            }
        }

        return distances / 2;
    }
    
    private static List<List<Point>> ParseLines(List<string> lines)
    {
        return lines.Select((line, y) =>
            line.ToCharArray().Select((c, x) => new Point(x, y, c == '#' ? PointValue.Galaxy : PointValue.Empty)).ToList()).ToList();
    }
    
    private static List<List<IShape>> ExpandMap(List<List<Point>> map)
    {
        List<List<Point>> columns = [];
        for (var x = 0; x < map[0].Count; x++)
        {
            columns.Add([]);
            for (var y = 0; y < map.Count; y++)
            {
                columns[x].Add(map.ElementAt(y).ElementAt(x));
            }
        }
        
        var emptyColumnsIndices = columns.Select((column, index) => (column, index)).Where(column => column.column.All(point => point.Value == PointValue.Empty)).Select(column => column.index).ToList();

        var currentY = 0;
        var expandedMap = map.SelectMany((line, y) =>
        {
            var currentX = 0;
            var newLine = line.SelectMany((point, x) =>
            {
                if (emptyColumnsIndices.Contains(x))
                    return (List<IShape>) [point with { X = currentX++, Y=currentY }, new HorizontalGap(currentX++, 1)];
                else
                    return new List<IShape> { point with{ X = currentX++, Y=currentY} };
            }).ToList();
            
            if (line.All(point => point.Value == PointValue.Empty))
            {
                var listOfVerticalGaps = line.Select(_ => (IShape) new VerticalGap(currentY + 1, 1)).ToList();
                currentY += 2;
                return new List<List<IShape>> {newLine, listOfVerticalGaps};
            }
            else
            {
                currentY += 1;
                return new List<List<IShape>> { newLine };
            }
        }).ToList();

        return expandedMap;
    }
    private static List<List<IShape>> ExpandMap2(List<List<Point>> map)
    {
        List<List<Point>> columns = [];
        for (var x = 0; x < map[0].Count; x++)
        {
            columns.Add([]);
            for (var y = 0; y < map.Count; y++)
            {
                columns[x].Add(map.ElementAt(y).ElementAt(x));
            }
        }
        
        var emptyColumnsIndices = columns.Select((column, index) => (column, index)).Where(column => column.column.All(point => point.Value == PointValue.Empty)).Select(column => column.index).ToList();
        var currentY = 0;
        var expandedMap = map.SelectMany((line, y) =>
        {
            int currentX = 0;
            List<IShape> newLine = line.SelectMany((point, x) =>
            {
                if (emptyColumnsIndices.Contains(x))
                {
                    var p = point with { X = currentX++, Y = currentY };
                    currentX += 999_999;
                    return [p, new HorizontalGap(currentX, 99)];
                }
                else
                {
                    return new List<IShape> { point with { X = currentX++, Y = currentY } };
                }
                    
            }).ToList();
            
            if (line.All(point => point.Value == PointValue.Empty))
            {
                var listOfVerticalGaps = line.Select(_ => (IShape) new VerticalGap(y, 99)).ToList();
                currentY += 1_000_000;
                
                return new List<List<IShape>> {newLine, listOfVerticalGaps};
            }
            else
            {
                currentY += 1;
                return new List<List<IShape>> { newLine };
            }
        }).ToList();

        return expandedMap;
    }
}