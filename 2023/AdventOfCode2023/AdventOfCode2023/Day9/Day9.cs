namespace AdventOfCode2023.Day9;

public static class Day9
{
    public static long Part1(IEnumerable<string> lines)
    {
        return Result(lines, LastItem);
    }

    public static long Part2(IEnumerable<string> lines)
    {
        return Result(lines, FirstItem);
    }

    private static long Result(IEnumerable<string> lines, Func<List<List<long>>, long> getItem)
    {
        var numbersLines = lines.Select(line => line.Split(" ").Select(long.Parse).ToList()).ToList();
        var result = numbersLines.Select(numbers =>
        {
            var diffListsWithNumbers = DiffListsWithNumbers(numbers);
            return getItem(diffListsWithNumbers);
        }).Sum();

        return result;
    }
    
    private static long LastItem(List<List<long>> diffListsWithNumbers)
    {
        long lastItem = 0;
        diffListsWithNumbers.Last().Add(lastItem);

        for (var i = diffListsWithNumbers.Count - 1; i >= 0; i--)
        {
            var diffList = diffListsWithNumbers.ElementAt(i);
            var diffListLastItem = diffList.Last();
            var newLastItem = diffListLastItem + lastItem;
            diffList.Add(newLastItem);
            lastItem = newLastItem;
        }

        return lastItem;
    }

    private static long FirstItem(List<List<long>> diffListsWithNumbers)
    {
        long firstItem = 0;
        diffListsWithNumbers.Last().Insert(0, firstItem);

        for (var i = diffListsWithNumbers.Count - 1; i >= 0; i--)
        {
            var diffList = diffListsWithNumbers.ElementAt(i);
            var diffListFirstItem = diffList.First();
            var newFirstItem = diffListFirstItem - firstItem;
            diffList.Insert(0, newFirstItem);
            firstItem = newFirstItem;
        }

        return firstItem;
    }

    private static List<List<long>> DiffListsWithNumbers(List<long> numbers)
    {
        var diffListsWithNumbers = new List<List<long>> { numbers };
        do
        {
            var newDiffList = new List<long>();
            var currentDiffList = diffListsWithNumbers.Last();
            for (var i = 0; i < currentDiffList.Count - 1; i++)
            {
                var num1 = currentDiffList.ElementAt(i);
                var num2 = currentDiffList.ElementAt(i + 1);
                var diff = num2 - num1;
                newDiffList.Add(diff);
            }
            diffListsWithNumbers.Add(newDiffList);
        } while (diffListsWithNumbers.Last().Any(num => num != 0));

        return diffListsWithNumbers;
    }
}