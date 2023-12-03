using System.Text;

namespace AdventOfCode2023.Day3;

public record Cell
{
    public char Value { get; set; }
    public int X { get; set; }
    public int Y { get; set; }
    
    public override string ToString()
    {
        return $"({X},{Y}) = {Value}";
    }
}

public record Number
{
    public int Value { get; set; }
    public IEnumerable<Cell> Neighbors { get; set; }
    public override string ToString()
    {
        return $"Number: {Value}, Neighbors: {Neighbors.Count()}";
    }
}

public static class Day3
{
    private const char EmptyCell = '.';
    private const char Gear = '*';

    public static int Part1(IEnumerable<string> lines)
    {
        var numbers = GetBoardNumbers(lines);
        
        return numbers.Where(num => num.Neighbors.FirstOrDefault(cell => IsSymbol(cell.Value)) != null).Select(num => num.Value).Sum();
    }

    public static int Part2(IEnumerable<string> lines)
    {
        var numbers = GetBoardNumbers(lines);
        var groupedByCommonGear = numbers.GroupBy(num =>
        {
            var found = num.Neighbors.FirstOrDefault(cell => cell.Value == Gear);
            return found != null ? $"HasGear-{found}" : "NoGear";
        });

        return groupedByCommonGear.Where(group => group.Key != "NoGear" && group.Count() > 1)
            .Select(group => group.Select(num => num.Value).Aggregate(1, (acc, curr) => acc * curr)).Sum();
    }

    private static bool IsSymbol(char c)
    {
        return !(char.IsNumber(c) || c == EmptyCell);
    }

    private static IEnumerable<Number> GetBoardNumbers(IEnumerable<string> lines)
    {
        var board = lines.Select((line, y) => line.ToCharArray().Select((c, x) => new Cell { Value = c, X = x, Y = y }));
        var numbers = board.SelectMany(row =>
        {
            var numbers = new List<Number>();
            var numberBuffer = new StringBuilder();
            var neighbours = new List<Cell>();
            var collectingNumber = false;
            foreach (var cell in row)
            {
                if ((cell.Value == EmptyCell || IsSymbol(cell.Value)) && !collectingNumber) continue;
                if ((cell.Value == EmptyCell || IsSymbol(cell.Value)) && collectingNumber)
                {
                    collectingNumber = false;
                    numbers.Add(new Number { Value = int.Parse(numberBuffer.ToString()), Neighbors = neighbours });
                    neighbours = new List<Cell>();
                    numberBuffer.Clear();
                    continue;
                }
                
                if (char.IsNumber(cell.Value) && collectingNumber)
                {
                    numberBuffer.Append(cell.Value);
                    neighbours.AddRange(GetNeighbors(board, cell, neighbours));
                }
                if (!char.IsNumber(cell.Value) || collectingNumber) continue;
                
                collectingNumber = true;
                numberBuffer.Append(cell.Value);
                neighbours.AddRange(GetNeighbors(board, cell, neighbours));
            }

            if (collectingNumber)
            {
                numbers.Add(new Number { Value = int.Parse(numberBuffer.ToString()), Neighbors = neighbours });
            }

            return numbers;
        });

        return numbers;
    }
    
    private static IEnumerable<Cell> GetNeighbors(IEnumerable<IEnumerable<Cell>> board, Cell cell, IEnumerable<Cell> currentNeighbours)
    {
        var neighborsVectors = new List<(int, int)>() { (-1,-1), (0,-1), (1,-1), (-1,0), (1,0), (-1,1), (0,1), (1,1) };
        var neighbors = neighborsVectors.Select(vector =>
        {
            var neighborCell = board.ElementAtOrDefault(cell.Y + vector.Item2)
                ?.ElementAtOrDefault(cell.X + vector.Item1);
            
            return neighborCell != null && (!char.IsNumber(neighborCell.Value)) &&
                   !currentNeighbours.Contains(neighborCell)
                ? neighborCell
                : null;
        }).OfType<Cell>();
        
        return neighbors;
    }
}