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
        var neighbors = new List<Cell>();
        var x = cell.X;
        var y = cell.Y;
        var left = x - 1;
        var right = x + 1;
        var top = y - 1;
        var bottom = y + 1;
        var leftCell = board.ElementAtOrDefault(y)?.ElementAtOrDefault(left);
        var rightCell = board.ElementAtOrDefault(y)?.ElementAtOrDefault(right);
        var topCell = board.ElementAtOrDefault(top)?.ElementAtOrDefault(x);
        var bottomCell = board.ElementAtOrDefault(bottom)?.ElementAtOrDefault(x);
        var topLeftCell = board.ElementAtOrDefault(top)?.ElementAtOrDefault(left);
        var topRightCell = board.ElementAtOrDefault(top)?.ElementAtOrDefault(right);
        var bottomLeftCell = board.ElementAtOrDefault(bottom)?.ElementAtOrDefault(left);
        var bottomRightCell = board.ElementAtOrDefault(bottom)?.ElementAtOrDefault(right);
        if (leftCell != null && (!char.IsNumber(leftCell.Value)) && !currentNeighbours.Contains(leftCell)) neighbors.Add(leftCell);
        if (rightCell != null && (!char.IsNumber(rightCell.Value)) && !currentNeighbours.Contains(rightCell)) neighbors.Add(rightCell);
        if (topCell != null && (!char.IsNumber(topCell.Value)) && !currentNeighbours.Contains(topCell)) neighbors.Add(topCell);
        if (bottomCell != null && (!char.IsNumber(bottomCell.Value)) && !currentNeighbours.Contains(bottomCell)) neighbors.Add(bottomCell);
        if (topLeftCell != null && (!char.IsNumber(topLeftCell.Value)) && !currentNeighbours.Contains(topLeftCell)) neighbors.Add(topLeftCell);
        if (topRightCell != null && (!char.IsNumber(topRightCell.Value)) && !currentNeighbours.Contains(topRightCell)) neighbors.Add(topRightCell);
        if (bottomLeftCell != null && (!char.IsNumber(bottomLeftCell.Value)) && !currentNeighbours.Contains(bottomLeftCell)) neighbors.Add(bottomLeftCell);
        if (bottomRightCell != null && (!char.IsNumber(bottomRightCell.Value)) && !currentNeighbours.Contains(bottomRightCell)) neighbors.Add(bottomRightCell);
        
        return neighbors;
    }
}