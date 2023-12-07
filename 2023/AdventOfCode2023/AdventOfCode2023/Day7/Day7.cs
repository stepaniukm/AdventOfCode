namespace AdventOfCode2023.Day7;

internal class Card(int value) : IEquatable<Card>, IComparable<Card>
{
    public int Value { get; set; } = value;

    public bool Equals(Card? other)
    {
        return Value == other?.Value;
    }

    public int CompareTo(Card? other)
    {
        return Value - other?.Value ?? 0;
    }

    public override string ToString()
    {
        return $"Card({Value})";
    }
};

internal class Hand(string name, List<Card> cards, int bid) : IEquatable<Hand>, IComparable<Hand>
{
    public static List<string> RanksList = new List<string>
        { "FiveOfAKind", "FourOfAKind", "FullHouse", "ThreeOfAKind", "TwoPair", "OnePair", "HighCard" };

    public List<Card> Cards { get; set; } = cards;
    public int Bid { get; set; } = bid;
    public string Name { get; set; } = name;

    public bool Equals(Hand? other)
    {
        if (other?.Name != Name) return false;
        if (other?.Cards.Count != Cards.Count) return false;

        return Cards.Select(((card, i) => new { card, i })).All(card =>
        {
            var otherCard = other?.Cards?.ElementAt(card.i);
            return otherCard == card.card;
        });
    }

    public int CompareTo(Hand? other)
    {
        var rank = RanksList.IndexOf(Name);
        var otherRank = RanksList.IndexOf(other?.Name);
        if (rank != otherRank) return rank - otherRank;

        var currentIndex = 0;

        do
        {
            var currentCardA = Cards[currentIndex];
            var currentCardB = other?.Cards[currentIndex];

            if (currentCardA.Value != currentCardB?.Value) return currentCardB.Value - currentCardA?.Value ?? 0;
            currentIndex++;
        } while (currentIndex < Cards.Count);

        return 0;
    }

    public override string ToString()
    {
        var cardString = cards.Aggregate("", (current, card) => current + card.ToString() + " ").Trim();
        return $"{Name} {cardString} {Bid}";
    }
};

public static class Day7
{
    public static int Part1(IEnumerable<string> lines)
    {
        var parsedLines = ParseLines(lines);
        parsedLines.Sort((hand, hand1) => hand.CompareTo(hand1));

        var result = parsedLines.Select(((hand, i) => hand.Bid * (parsedLines.Count - i))).Sum();

        return result;
    }

    public static int Part2(IEnumerable<string> lines)
    {
        var parsedLines = ParseLines2(lines);
        parsedLines.Sort((hand, hand1) => hand.CompareTo(hand1));

        var result = parsedLines.Select(((hand, i) => hand.Bid * (parsedLines.Count - i))).Sum();

        return result;
    }


    private static List<Hand> ParseLines(IEnumerable<string> lines)
    {
        return lines.Select(line =>
        {
            var split = line.Split(" ");
            var bid = int.Parse(split.Last());
            var cards = split.First().ToCharArray().Select(s =>
            {
                return s switch
                {
                    'A' => new Card(14),
                    'K' => new Card(13),
                    'Q' => new Card(12),
                    'J' => new Card(11),
                    'T' => new Card(10),
                    _ => new Card(int.Parse(s + ""))
                };
            }).ToList();
            var cardsGroupedByValue = cards.GroupBy(card => card.Value).ToList();

            var counts = cardsGroupedByValue.Select(group => group.Count()).ToList();

            if (counts.Contains(5)) return new Hand("FiveOfAKind", cards, bid);
            if (counts.Contains(4)) return new Hand("FourOfAKind", cards, bid);
            if (counts.Contains(3) && counts.Contains(2)) return new Hand("FullHouse", cards, bid);
            if (counts.Contains(3)) return new Hand("ThreeOfAKind", cards, bid);
            if (counts.Count(c => c == 2) == 2) return new Hand("TwoPair", cards, bid);
            if (counts.Contains(2)) return new Hand("OnePair", cards, bid);

            return new Hand("HighCard", cards, bid);
        }).ToList();
    }

    private static List<Hand> ParseLines2(IEnumerable<string> lines)
    {
        return lines.Select(line =>
        {
            var split = line.Split(" ");
            var bid = int.Parse(split.Last());
            var cards = split.First().ToCharArray().Select(s =>
            {
                return s switch
                {
                    'A' => new Card(14),
                    'K' => new Card(13),
                    'Q' => new Card(12),
                    'T' => new Card(10),
                    'J' => new Card(1),
                    _ => new Card(int.Parse(s + ""))
                };
            }).ToList();
            var cardsGroupedByValue = cards.GroupBy(card => card.Value).ToList();
            
            var jokersCount = cardsGroupedByValue.FirstOrDefault(g => g.Key == 1, null)?.Count() ?? 0;
            var countsWithoutJokers =
                cardsGroupedByValue.Where(c => c.Key != 1).Select(group => group.Count()).ToList();
            countsWithoutJokers.Sort((a, b) => b - a);
            if (countsWithoutJokers.Count == 0) countsWithoutJokers.Add(0);
            countsWithoutJokers[0] += jokersCount;

            if (countsWithoutJokers.Contains(5)) return new Hand("FiveOfAKind", cards, bid);
            if (countsWithoutJokers.Contains(4)) return new Hand("FourOfAKind", cards, bid);
            if (countsWithoutJokers.Contains(3) && countsWithoutJokers.Contains(2)) return new Hand("FullHouse", cards, bid);
            if (countsWithoutJokers.Contains(3)) return new Hand("ThreeOfAKind", cards, bid);
            if (countsWithoutJokers.Count(c => c == 2) == 2) return new Hand("TwoPair", cards, bid);
            if (countsWithoutJokers.Contains(2)) return new Hand("OnePair", cards, bid);

            return new Hand("HighCard", cards, bid);
        }).ToList();
    }
}