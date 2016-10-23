%Name: Derui Wang
%Student ID: 679552
%Purpose: This game intial with a puzzle that includes '_' and '#' characters. There will
%         be a Wordlist that provided to be filled into it with the shortest time. My method
%         mainly includes three parts.
%Method:
%         (1) change "_" with logical variables
%         (2) combine "_" to be a list of slots that can be filled into
%         (3) find the best slots that have the least number of possibilities in the 
%             word list and try to fill these slots firstly to save time.



%load the functions transpose for later use
:- ensure_loaded(library(clpfd)).

%----------------------------------Predefined functions start-------------------------%
%Main program to start the game
main(PuzzleFile, WordlistFile, SolutionFile) :-

   %read the puzzle file
    read_file(PuzzleFile, Puzzle),

    %read the word list file
    read_file(WordlistFile, Wordlist),

    % Ensure the puzzle is actually valid
    %check if each row inside the puzzle is in same lenght
    valid_puzzle(Puzzle),

    %Including functions to actually solve the problem
    solve_puzzle(Puzzle, Wordlist),

    %print out the solution
    print_puzzle(SolutionFile, Puzzle).


%open the specified file by Filename 
%By using read_lines/2 to read contents in the file line
%by line and store them into Content
%close the reading stream after reading files
read_file(Filename, Content) :-
    open(Filename, read, Stream),
    read_lines(Stream, Content),
    close(Stream).


%read all lines and store the content by helper function
%read_line/3. When it reaches the last part of the file
%store all lines' content into Content
read_lines(Stream, Content) :-
    read_line(Stream, Line, Last),
    (   Last = true
    ->  (   Line = []
        ->  Content = []
        ;   Content = [Line]
        )
    ;  Content = [Line|Content1],
        read_lines(Stream, Content1)
    ).


%read single line and used by read_lines to store content

%when it reaches the end of the file. change Last to true, then return 
%it back to read_lines/2 to store content and stop recursion.

%when it reaches the end of one line, change Last to false, then return 
%it back to read_lines/2 to keep looping the next line.

%modified the old read_lines/3 by adding new conditions when
%Char = "_". Implement hint 3 to replace "_" with a logical variable. keep
%looping this line until it reaches '\n'.

%when it reaches the characters other than '_' which are '#' or normal charactesr
%, keep looping and store them into Line until it reaches '\n'. 
read_line(Stream, Line, Last) :-
    get_char(Stream, Char),
    (   Char = end_of_file
    ->  Line = [],
        Last = true
    ; Char = '\n'
    ->  Line = [],
        Last = false
    ; Char = '_'
    ->  Line = [LogicalVariable|Line1],
        read_line(Stream, Line1, Last)
    ; Line = [Char|Line1],
        read_line(Stream, Line1, Last)
    ).


%print the solved puzzle into SolutionFile
%use bulidin maplist function and helper function print_row
%to print the sovled puzzle line by line
%close the stream after writing files.
print_puzzle(SolutionFile, Puzzle) :-
    open(SolutionFile, write, Stream),
    maplist(print_row(Stream), Puzzle),
    close(Stream).


%print each row with helper functino maplsit to print
%charaters one by one
print_row(Stream, Row) :-
    maplist(put_puzzle_char(Stream), Row),
    nl(Stream).


%put '_' in the writer stream if it is still a 
%logical variable. Otherwise put the guessed Char into it
put_puzzle_char(Stream, Char) :-
    (   var(Char)
    ->  put_char(Stream, '_')
    ;   put_char(Stream, Char)
    ).


%check if the puzzle solution is in the right fomart
%use helper function samelength
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
    maplist(samelength(Row), Rows).


%check if both puzzle has same length
samelength([], []).
samelength([_|L1], [_|L2]) :-
    same_length(L1, L2).

%----------------------------------Predefined functions end-------------------------%



%--------------------------------Solve_puzzle functions start-------------------------%

solve_puzzle(Puzzle, WordList) :-
    %fomulate a list of slots called HorizontalSlots. Put the existing
    %logical variable in a Horizontal Slots list using hint 4
    fomulate_slots(Puzzle, [], HorizontalSlots),

    %use loaded function transpose/2 to transpose horizontal puzzle into 
    %a vertical puzzle
    transpose(Puzzle, VerticalPuzzle),

    %fomulate a list of slots called VerticalSlots based on HorizontalSlots
    %which means the existing slots with predifined logical variable would
    %not change. Using hint 4.
    fomulate_slots(VerticalPuzzle, HorizontalSlots, VerticalSlots),

    %try to fill the word into processed slots 
    fill_slots(VerticalSlots, WordList).
%--------------------------------Solve_puzzle functions end--------------------------%



%------------------------Slot builder helper functions start-------------------------%
%fomulate slots by helper function process_row_slots/4.
%e.g. ['LogicalVariable','LogicalVariable','#','LogicalVariable','LogicalVariable']
%e.g output  to [['LogicalVariable','LogicalVariable'],['LogicalVariable','LogicalVariable']]
%loop row by row in Puzzle. when it reaches the end of file, the current slots 
%will be the final slots list.
fomulate_slots([], Slots, Slots).
fomulate_slots([Row|Rows], Slots, NewSlots):-
    process_slots(Row, [], Slots, CurrentSlots),
    fomulate_slots(Rows, CurrentSlots, NewSlots).


%define the predicate when the row is an empty list if the length of this word 
%is larger than 1 then append this word with previous slots. the output CurrentSlots 
%is final output slots 
process_slots([], Word, Slots, CurrentSlots):-
    (length(Word, LenOfWord),
    LenOfWord > 1 ->
    append(Slots,[Word], CurrentSlots)
    ;
    Slots = CurrentSlots
    ).


%define the predicate when the row is not an empty list
%if the the character in this row is a '#' and the word is currently an empty list
%keep looping the next character in this row. If the word is not an empty list then 
%check if the word is a real word and update the slots. if the word is not a real 
%word, keep loopnig the next character in this row.

%if the character is not a '#', then append this character with the word list and 
%keep looping to check next character
process_slots([Head|Tail], Word, Slots, CurrentSlots):-
    Head == '#' ->
    (Word == [] ->
    process_slots(Tail,[], Slots, CurrentSlots)
    ;
        length(Word, LenOfCharacter),
         LenOfCharacter > 1 ->
         append(Slots, [Word], UpdateSlots),
         process_slots(Tail,[],UpdateSlots, CurrentSlots)
         ;
         process_slots(Tail,[],Slots,CurrentSlots)
        )
    ;
    append(Word,[Head], UpdateWord),
    process_slots(Tail, UpdateWord, Slots, CurrentSlots).
%------------------------Slot builder helper functions end-------------------------%



%------------------------fill the slots function start-----------------------------%
%start to fill the slots
fill_slots([],[]).
fill_slots(FinalSlots,Wordlist):-
    %choose the slot with the least number of possibilities in the worldlist to fill
    %in firstly
    choose_best_slot_tofill(FinalSlots,Wordlist,BestSlot),

    %generate possible words list after finding the best slots by matching the best slot
    %with the words in the words list
    exclude(\=(BestSlot), Wordlist, PossibleWords),

    %pick a word that in the PossibleWords list and fill the slot with this word
    member(Word,PossibleWords),
    BestSlot = Word,

    %delete this word in the wordlist in case repetition
    exclude(==(Word),Wordlist,WordsLeft),
    %delte this slot in the puzzle slot in case repeition
    exclude(==(BestSlot),FinalSlots,NextSlots),

    %keep looping and fill in other slots with the left words
    fill_slots(NextSlots,WordsLeft).


%by given the slots list, find the best slot which is the slot that has the 
%least number of possibilities in the worldlist to fill in firstly. To achieve this
%we need to first count the possibilities for each slots in the FinalSlots list first
choose_best_slot_tofill([Head|Tail],Wordlist,BestSlot):-
    %count the possibilities
    choose_word(Head,Wordlist,Count),
    %base on the count, find the best slot to start with
    choose_best_slot_tofill(Tail,Wordlist,Count,Head,BestSlot).

%if there is no more slots in FinalSlots, the current finding BestSlot is the 
%best slot to start with
choose_best_slot_tofill([],_,_,BestSlot,BestSlot).
%Start to loop each slots in the final slots and store the count in the Minimum variable
%if there is any slots that have possibilities nubmer that smaller than the minumum one
%update it with the new count number and update the CurrentBestSlot 
choose_best_slot_tofill([Head|Tail],Wordlist,Minimum,CurrentBestSlot,BestSlot):-
    choose_word(Head,Wordlist,Count),
    (Count<Minimum ->
    UpdateCurrentBestSlot = Head,
    UpdateMinimum = Count
    ;
    UpdateCurrentBestSlot = CurrentBestSlot,
    UpdateMinimum = Minimum),
    choose_best_slot_tofill(Tail,Wordlist,UpdateMinimum,UpdateCurrentBestSlot,BestSlot).




%check the currrent slot e.g. ['LogicalVariable','LogicalVariable'] has how many Words
%that match this slot
choose_word(Slot,Wordlist,Count):-
    compute_possibilities(Slot,Wordlist,0,Count).


%if the worldlist reaches the end, the current count is the final count
compute_possibilities(_,[],Accumulator,Accumulator).
%use the logical variable to match each words in the wordlist, if there is a
%match, count increases by 1. keep looping to find all possibilities and save 
%the count number into Count
compute_possibilities(Slot,[Head|Tail],Accumulator,Count):-
    %logic variables can help match
    (Slot \= Head ->
    UpdateAccumulator is Accumulator + 0
    ;
    UpdateAccumulator is Accumulator + 1),
    compute_possibilities(Slot,Tail,UpdateAccumulator,Count).


%------------------------fill the slots function end-----------------------------%