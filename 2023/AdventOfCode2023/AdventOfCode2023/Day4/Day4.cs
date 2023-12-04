namespace AdventOfCode2023.Day4;

internal record Card(IEnumerable<int> WinningNumbers, IEnumerable<int> PlayerNumbers);

public static class Day4
{
    public static double Part1(IEnumerable<string> lines)
    {
        var result = GetCards(lines);

        var count = result.Select(card =>
        {
            var winningNumbers = card.WinningNumbers;
            var playerNumbers = card.PlayerNumbers;

            var matches = winningNumbers.Intersect(playerNumbers);
            var matchesCount = matches.Count();

            return matchesCount > 0 ? Math.Pow(2, matchesCount - 1) : 0;
        }).Sum();

        return count;
    }

    public static int Part2(IEnumerable<string> lines)
    {
        var cards = GetCards(lines);
        var cardsMultipliers = new Dictionary<int, int>();

        var result = cards.Select((card, i) =>
        {
            var collectionBooster = cardsMultipliers.GetValueOrDefault(i, 1);
            var newScratchcards = card.PlayerNumbers.Intersect(card.WinningNumbers).Count();
            var init = i + 1;
            for (var j = init; j < init + newScratchcards; j++)
            {
                cardsMultipliers[j] = cardsMultipliers.GetValueOrDefault(j, 1) + collectionBooster;
            }

            return collectionBooster;
        }).Sum();

        return result;
    }

    private static IEnumerable<Card> GetCards(IEnumerable<string> lines)
    {
        var result = lines.Select(line =>
        {
            var splitByColon = line.Split(": ");
            var gameLine = splitByColon.Last();
            var gameParts = gameLine.Trim().Split(" | ");

            var winningNumbers = gameParts.First().Trim().Split(" ").Where(num => num != "").Select(int.Parse);
            var playerNumbers = gameParts.Last().Trim().Split(" ").Where(num => num != "").Select(int.Parse);

            var card = new Card(WinningNumbers: winningNumbers, PlayerNumbers: playerNumbers);

            return card;
        });
        return result;
    }
}