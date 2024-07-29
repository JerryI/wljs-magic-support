BeginPackage["Notebook`Editor`JSProcessor`", {
    "JerryI`Notebook`", 
    "JerryI`Notebook`Evaluator`", 
    "JerryI`Notebook`Kernel`", 
    "JerryI`Notebook`Transactions`",
    "JerryI`Misc`Events`"
}]

Begin["`Internal`"]

FileReadQ[t_Transaction] := (StringMatchQ[t["Data"], RegularExpression["^[\\w|\\d|\\-|\\_]*\\.[\\w]+$"]  ] )
filereader  = StandardEvaluator["Name" -> "File Reader", "InitKernel" -> init, "Pattern" -> (_?FileReadQ), "Priority"->(9)];
StandardEvaluator`ReadyQ[filereader, k_] := True;
StandardEvaluator`Evaluate[filereader, k_, t_] := Module[{path, imported},
    path = FileNameJoin[{Notebook`HashMap[ t["EvaluationContext", "Notebook"] ]["Path"] // DirectoryName, StringTrim[ t["Data"] ]}];
    If[!FileExistsQ[path], 
        EventFire[t, "Error", "File "<>path<>" does not exist"];
        Return[$Failed];
    ];

    imported = Import[path, "String"];
    If[ByteCount[imported] > 30000,
        EventFire[t, "Error", "File "<>path<>" is too large to be displayed"];
        Return[$Failed];        
    ];

    EventFire[t, "Result", <|"Data"->imported, "Meta"->Sequence["Display"->"fileprint"]|>];
    EventFire[t, "Finished", True];    
];  


ImageReadQ[t_Transaction] := (StringMatchQ[t["Data"], RegularExpression["^[\\w|\\d|\\-|\\_]*\\.(png|jpg|jpeg|gif|svg|bmp|ttf)$"]  ] )
imagereader  = StandardEvaluator["Name" -> "Image Reader", "InitKernel" -> init, "Pattern" -> (_?ImageReadQ), "Priority"->(8)];
StandardEvaluator`ReadyQ[imagereader, k_] := True;
StandardEvaluator`Evaluate[imagereader, k_, t_] := Module[{path, reducedPath},
    path = FileNameJoin[{Notebook`HashMap[ t["EvaluationContext", "Notebook"] ]["Path"] // DirectoryName, StringTrim[ t["Data"] ]}];
    If[!FileExistsQ[path], 
        EventFire[t, "Error", "File "<>path<>" does not exist"];
        Return[$Failed];
    ];

    reducedPath = StringTrim[ t["Data"] ];

    EventFire[t, "Result", <|"Data"->reducedPath, "Meta"->Sequence["Display"->"image"]|>];
    EventFire[t, "Finished", True];    
];  


FileWriteQ[t_Transaction] := (StringMatchQ[t["Data"], RegularExpression["^[\\w|\\d|\\-|\\_]*\\.[\\w]+\\n"]~~__  ] )
filewriter  = StandardEvaluator["Name" -> "File Writer", "InitKernel" -> init, "Pattern" -> (_?FileWriteQ), "Priority"->(11)];
StandardEvaluator`ReadyQ[filewriter, k_] := True;
StandardEvaluator`Evaluate[filewriter, k_, t_] := Module[{path},
    path = FileNameJoin[{Notebook`HashMap[ t["EvaluationContext", "Notebook"] ]["Path"] // DirectoryName, StringCases[t["Data"], RegularExpression["^([\\w|\\d|\-|\\_]*\\.[\\w]+)\\n"] -> "$1"] // First // StringTrim }];

    If[FileExistsQ[path], DeleteFile[path] ];
    WriteString[path, StringReplace[t["Data"], RegularExpression["^([\\w|\\d|\-|\\_]*\\.[\\w]+)\\n"] -> ""] ];

    EventFire[t, "Result", <|"Data"->StringTemplate["``"][path], "Meta"->Sequence["Display"->"fileprint"]|>];
    EventFire[t, "Finished", True];    
];  

init[k_] := Null;


End[]

EndPackage[]
