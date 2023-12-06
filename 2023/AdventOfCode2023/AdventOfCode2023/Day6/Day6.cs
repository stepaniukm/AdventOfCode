namespace AdventOfCode2023.Day6;

internal record Race(ulong distance, ulong time);
public static class Day6
{
    private const ulong A = 1;
    public static ulong Part1(IEnumerable<string> lines)
    {
        var combinedRaces = GetCombinedRaces(lines);
        var result = combinedRaces.Select(race =>
        {
            var better = Enumerable.Range(1, (int) race.time - 1).Select(pressingTime =>
            {
                var v = A * (ulong) pressingTime;
                var s = v * (race.time - (ulong) pressingTime);

                return s > race.distance ? s : 0;
            }).Count(n => n != 0);
            
            return better;
        }).Aggregate(1, (i, i1) => i * i1);

        return (ulong) result;
    }

    

    public static uint Part2(IEnumerable<string> lines)
    {
        var race = GetCombinedRacesPart2(lines);
        
        var better = Enumerable.Range(1, (int) race.time - 1).Select(pressingTime =>
        {
            var v = A * (ulong)pressingTime;
            var s = v * (race.time - (ulong)pressingTime);

            return s > race.distance ? s : 0;
        }).Count(n => n != 0);
        
        return (uint) better;
    }
    
    private static IEnumerable<Race> GetCombinedRaces(IEnumerable<string> lines)
    {
        var timesLine = lines.First();
        var distancesLine = lines.Last();
        var times = timesLine.Split(": ").Last().Trim().Split(" ").Where(l => l!="").Select(int.Parse);
        var distances = distancesLine.Split(": ").Last().Trim().Split(" ").Where(l => l!="").Select(int.Parse);
        var combinedRaces = times.Select(((time, i) =>
        {
            var distance = distances.ElementAt(i);
            return new Race((uint)distance, (uint)time);
        }));
        return combinedRaces;
    }
    
    private static Race GetCombinedRacesPart2(IEnumerable<string> lines)
    {
        var timesLine = lines.First();
        var distancesLine = lines.Last();
        var time = timesLine.Split(": ").Last().Trim().Split(" ").Where(l => l != "")
            .Aggregate("", (s, s1) => s + s1 + "");
        var distance = distancesLine.Split(": ").Last().Trim().Split(" ").Where(l => l!="").Aggregate("", (s, s1) => s + s1 + "");
        
        return new Race(ulong.Parse(distance), ulong.Parse(time));
    }
}