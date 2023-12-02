using System.Text;

namespace AdventOfCode2023.Day1;

public static class Day1
{
    public static int Part1(IEnumerable<string> lines)
    {
        var result = lines.Select(line =>
        {
            var charIndices = (
                from item in line
                where char.IsNumber(item)
                select item
            ).ToList();

            var first = charIndices.First();
            var last = charIndices.Last();

            var num = int.Parse($"{first}{last}");

            return num;
        }).Sum();

        return result;
    }


    private static List<string?> PrepareLine(string line)
    {
        var stringToNum = new Dictionary<string, string>
        {
            { "one", "1" },
            { "two", "2" },
            { "three", "3" },
            { "four", "4" },
            { "five", "5" },
            { "six", "6" },
            { "seven", "7" },
            { "eight", "8" },
            { "nine", "9" },
            { "zero", "0" }
        };


        var parseBuffersToNumbers = Enumerable
            .Range(0, line.Length)
            .Select(item => line[item..])
            .Select(buffer =>
            {
                var str = buffer.ToString();
                var maybeCharNum = str.First();

                if (char.IsNumber(maybeCharNum))
                {
                    return maybeCharNum.ToString();
                }

                var foundKey = stringToNum.Keys.FirstOrDefault(key => str.StartsWith(key));
                return foundKey != null ? stringToNum.GetValueOrDefault(foundKey) : null;
            }).Where(c => c != null).ToList();

        return parseBuffersToNumbers;
    }
    
    public static int Part2(List<string> lines)
    {
        var result = lines.ConvertAll(line =>
        {
            var preparedLine = PrepareLine(line);

            var firstItem = preparedLine.First();
            var lastItem = preparedLine.Last();

            var num = int.Parse($"{firstItem}{lastItem}");

            return num;
        }).Sum();

        return result;
    }
}