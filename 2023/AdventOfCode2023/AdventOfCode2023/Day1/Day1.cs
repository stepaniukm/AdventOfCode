using System.Text;

namespace AdventOfCode2023.Day1;

public class Day1
{
    public int Part1(List<string> lines)
    {
        var result = lines.ConvertAll((line) =>
        {
            var charIndices = new List<int>();
            foreach (var item in line.Select(((c, i) => new { c, i })))
            {
                if (Char.IsNumber(item.c))
                {
                    charIndices.Add(item.i);
                }
            }

            var first = charIndices.First();
            var last = charIndices.Last();

            var firstItem = line[first];
            var lastItem = line[last];

            var num = Int32.Parse($"{firstItem}{lastItem}");
            
            return num;
        }).Sum();

        return result;
    }


    private List<string> PrepareLine(string line)
    {
        Dictionary<string, string> stringToNum = new Dictionary<string, string>();
        stringToNum.Add("one", "1");
        stringToNum.Add("two", "2");
        stringToNum.Add("three", "3");
        stringToNum.Add("four", "4");
        stringToNum.Add("five", "5");
        stringToNum.Add("six", "6");
        stringToNum.Add("seven", "7");
        stringToNum.Add("eight", "8");
        stringToNum.Add("nine", "9");
        stringToNum.Add("zero", "0");
        
        var listOfBuffers = new List<StringBuilder>();
        
        foreach (var item in line)
        {
            var newBuffer = new StringBuilder();
            listOfBuffers.Add(newBuffer);
            listOfBuffers.ForEach((buffer) => buffer.Append(item));
        }

        var parseBuffersToNumbers = listOfBuffers.ConvertAll((buffer) =>
        {
            var str = buffer.ToString();

            var maybeCharNum = str[0];

            if (Char.IsNumber(maybeCharNum))
            {
                return maybeCharNum.ToString();
            }

            var foundKey = stringToNum.Keys.FirstOrDefault((key) => str.StartsWith(key));
            
            return stringToNum.GetValueOrDefault(foundKey);
        }).Where((c) => c != null).ToList();

        return parseBuffersToNumbers;
    }
    public int Part2(List<string> lines)
    {
        var result = lines.ConvertAll((line) =>
        {
            var preparedLine = PrepareLine(line);

            var firstItem = preparedLine.First();
            var lastItem = preparedLine.Last();

            var num = Int32.Parse($"{firstItem}{lastItem}");
            
            Console.WriteLine(num);
            
            return num;
        }).Sum();

        return result;
    }
}