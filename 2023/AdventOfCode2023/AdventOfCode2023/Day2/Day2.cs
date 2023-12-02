using System.Text;
using System.Text.RegularExpressions;

namespace AdventOfCode2023.Day2;

record Colors
{
    public int Red { get; set; }
    public int Green { get; set; }
    public int Blue { get; set; }

    public override string ToString()
    {
        return $"RGB({Red},{Green},{Blue})";
    }
}

public static class Day2
{
    private static IEnumerable<Colors> ParseLine(string line)
    {
        return line.Split(";").Select((game =>
        {
            const string greenRegex = @"(?<amount>\d+) green";
            var greenResult = Regex.Matches(game, greenRegex).ToList();
            var greenAmount = greenResult.Count > 0 ? int.Parse(greenResult.First().Groups["amount"].Value) : 0;

            const string redRegex = @"(?<amount>\d+) red";
            var redResult = Regex.Matches(game, redRegex).ToList();
            var redAmount = redResult.Count > 0 ? int.Parse(redResult.First().Groups["amount"].Value) : 0;

            const string blueRegex = @"(?<amount>\d+) blue";
            var blueResult = Regex.Matches(game, blueRegex).ToList();
            var blueAmount = blueResult.Count > 0 ? int.Parse(blueResult.First().Groups["amount"].Value) : 0;

            return new Colors { Red = redAmount, Blue = blueAmount, Green = greenAmount };
        }));
    }

    public static int Part1(IEnumerable<string> lines)
    {
        var startingConditions = new Colors { Red = 12, Green = 13, Blue = 14 };
        var result = lines.Select(((line, i) =>
        {
            var games = ParseLine(line);

            return games.All((game => game.Blue <= startingConditions.Blue && game.Red <= startingConditions.Red &&
                                      game.Green <= startingConditions.Green)) ? i + 1 : 0;
        })).Sum();

        return result;
    }


    public static int Part2(List<string> lines)
    {
        var result = lines.Select(((line, i) =>
        {
            var games = ParseLine(line).Aggregate(((acc, colors) =>
            {
                if (colors.Red > acc.Red) acc.Red = colors.Red;
                if (colors.Blue > acc.Blue) acc.Blue = colors.Blue;
                if (colors.Green > acc.Green) acc.Green = colors.Green;
                
                return acc;
            }));

            return games.Red * games.Blue * games.Green;

        })).Sum();

        return result;
    }
}