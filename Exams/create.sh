#!/bin/bash

echo "Creating $1"
mkdir $1
cd $1

dotnet new sln
dotnet new classlib --language F# -o Exam
dotnet new nunit --language F# -o Exam.Tests

dotnet add Exam.Tests reference Exam
dotnet sln add Exam
dotnet sln add Exam.Tests
