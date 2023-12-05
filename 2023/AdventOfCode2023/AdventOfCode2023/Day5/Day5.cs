using System.Xml;

namespace AdventOfCode2023.Day5;

public record MRange(ulong start, ulong length);

public record Mapping(string source, string destination, ulong sourceStart, ulong destinationStart, ulong length);

public static class Day5
{
    public static ulong Part1(IEnumerable<string> lines)
    {
        var seedsLine = lines.First();
        var seeds = seedsLine.Split(": ").Last().Split(" ").Select(ulong.Parse).ToList();
        var groupedByMapping = GetGroupedByMapping(lines);

        var result = seeds.AsParallel().Select(seed =>
        {
            var nextSource = "seed";
            var currentValue = seed;
            do
            {
                var source = nextSource;
                var currentMappings = groupedByMapping.Where(range => range.source == source);
                nextSource = currentMappings.First().destination;

                foreach (var range in currentMappings)
                {
                    if (currentValue < range.sourceStart || currentValue >= range.sourceStart + range.length) continue;
                    currentValue = range.destinationStart + currentValue - range.sourceStart;
                    break;
                }
            } while (nextSource != "location");

            return currentValue;
        }).Min();

        return result;
    }

    public static int Part2(IEnumerable<string> lines)
    {
        var seedsLine = lines.First();
        var seedsPairs = seedsLine.Split(": ").Last().Split(" ").Select(ulong.Parse).Chunk(2);
        var seedsRanges = seedsPairs.Select((pair) => new MRange(pair.First(), pair.Last()));
        var groupedByMapping = GetGroupedByMapping(lines);

        var result = Enumerable.Range(1, 100_000_000).First(num =>
        {
            var currentSource = "location";
            var currentValue = (ulong) num;
            
            do
            {
                var source = currentSource;
                var currentMappings = groupedByMapping.Where(range => range.destination == source);
                currentSource = currentMappings.First().source;
                
                foreach (var mapping in currentMappings)
                {
                    if (currentValue < mapping.destinationStart || currentValue >= mapping.destinationStart + mapping.length) continue;
                    currentValue = mapping.sourceStart + currentValue - mapping.destinationStart;
                    break;
                }
                
            } while (currentSource != "seed");
            
            return seedsRanges.Any(range => range.start <= currentValue && range.start + range.length - 1 >= currentValue);
        });

        return result;
    }

    private static List<Mapping> GetGroupedByMapping(IEnumerable<string> lines)
    {
        var currentSource = "";
        var currentDestination = "";

        var groupedByMapping = lines.Skip(1).Aggregate(new List<Mapping>(), (acc, line) =>
        {
            if (char.IsLetter(line.FirstOrDefault()))
            {
                var nameParts = line.Split(" ");
                var sourceAndDestination = nameParts.First().Split("-to-");
                var source = sourceAndDestination.First();
                var destination = sourceAndDestination.Last();

                currentSource = source;
                currentDestination = destination;
                return acc;
            }

            if (char.IsNumber(line.FirstOrDefault()))
            {
                var parts = line.Split(" ");
                var destinationStart = ulong.Parse(parts.First());
                var sourceStart = ulong.Parse(parts.Skip(1).First());
                var length = ulong.Parse(parts.Skip(2).First());

                acc.Add(new Mapping(currentSource, currentDestination, sourceStart, destinationStart, length));
                return acc;
            }

            return acc;
        });
        return groupedByMapping;
    }
}