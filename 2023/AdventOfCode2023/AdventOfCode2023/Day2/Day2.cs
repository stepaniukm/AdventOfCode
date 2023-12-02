using System.Text.RegularExpressions;

namespace AdventOfCode2023.Day2;

internal record Colors
{
    public int Red { get; set; }
    public int Green { get; set; }
    public int Blue { get; set; }

    public override string ToString()
    {
        return $"RGB({Red},{Green},{Blue})";
    }
}

public static partial class Day2
{
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
    public static int Part2(IEnumerable<string> lines)
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
    private static IEnumerable<Colors> ParseLine(string line)
    {
        return line.Split(";").Select((game =>
        {
            var greenAmount = GetAmount(GreenRegex(), game);
            var blueAmount = GetAmount(BlueRegex(), game);
            var redAmount = GetAmount(RedRegex(), game);

            return new Colors { Red = redAmount, Blue = blueAmount, Green = greenAmount };
        }));
    }

    private static int GetAmount(Regex r, string game)
    {
        var matches = r.Matches(game).ToList();
        return matches.Count > 0 ? int.Parse(matches.First().Groups["amount"].Value) : 0;
    }

    [GeneratedRegex(@"(?<amount>\d+) green")]
    private static partial Regex GreenRegex();
    
    [GeneratedRegex(@"(?<amount>\d+) red")]
    private static partial Regex RedRegex();
    
    [GeneratedRegex(@"(?<amount>\d+) blue")]
    private static partial Regex BlueRegex();
}