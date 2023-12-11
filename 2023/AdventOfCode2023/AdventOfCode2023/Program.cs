// See https://aka.ms/new-console-template for more information

using AdventOfCode2023.Day11;

var lines = File.ReadAllText("./Day11/input.txt").Split("\n").ToList();
var result = Day11.Part1(lines);

// var lines2 = File.ReadAllText("./Day10/test2.txt").Split("\n").ToList();
var result2 = Day11.Part2(lines);

Console.WriteLine("-------");
Console.WriteLine(result);
Console.WriteLine(result2);