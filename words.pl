% Read the first character of the next word with get0,
%    finish composing the word (W) using read_word,
%    finish composing the rest of the words in the
%       sentence (Ws) using rest_of_sentence
read_word_list([W|Ws]) :-
  get0(C),
  read_word(C, W, C1),       
  rest_of_sentence(C1, Ws), !. 

% Here we grab the rest of the sentence

% end_of_sentence tells us if we're at the end
%    (when we've hit a ! . ? or end-of-line)
rest_of_sentence(C,[]) :- end_of_sentence(C), !. 

% the general case is that we have to read the next word
%     and the rest of the sentence
%     (just as with read_word_list)
rest_of_sentence(C,[W1|Ws]) :-
  read_word(C,W1,C1),       
  rest_of_sentence(C1,Ws).


% Here we grab the rest of a word, storing it in W,
%    assuming the first character was read in in C
% We wind up with C1 being the first character
%    AFTER the current word's completion

% if C is a punctuation mark it is treated as a valid
%    word all by itself, so set W to contain just that character
read_word(C,W,C1) :-  
  single_char(C), !, 
  name(W, [C]),           
  get0(C1).

% if C is a valid character to appear in a "regular" word
%    (i.e. alphanumeric) then continue building the word
%    using rest_of_word and glue it together to form W
read_word(C,W,C2) :-        
  char_in_word(C, NewC),       
  get0(C1),              
  rest_of_word(C1,Cs,C2),   
  name(W, [NewC|Cs]).  

% otherwise C must be a seperator (pretty much anything not
%    covered above) so it's time to start a new word
read_word(_,W,C2) :-       
  get0(C1),       
  read_word(C1,W,C2).     

% rest_of_word checks that the latest character is valid for
%    the body of a word, tacks it on to our word-in-progress,
%    and continues
% We wind up with C2 being the first character
%    AFTER the current word's completion

rest_of_word(C, [NewC|Cs], C2) :-
  char_in_word(C, NewC),
  get0(C1),
  rest_of_word(C1, Cs, C2).

% our base/stopping case
rest_of_word(C, [], C).

% here we list all the characters that will be treated
%    as if they were words by themselves, 
% i.e. punctuation that doesn't appear in the middle of a word
single_char(0',).
single_char(0';).
single_char(0':).
single_char(0'?).
single_char(0'!).
single_char(0'.).

% here we list all the characters that can appear as a valid
%    part of a larger word, mostly alpha-numeric, but also dashes
%    and underscores
char_in_word(C, C) :- C >= 0'a, C =< 0'z.
char_in_word(C, C) :- C >= 0'0, C =< 0'9.
char_in_word(C, L) :- C >= 0'A, C =< 0'Z, L is C + 32.
char_in_word(0'-,0'-).
char_in_word(0'_,0'_).


% end_of_sentence checks if the character is the valid end of a 
%    sentence, i.e. a newline, . ! or ?

end_of_sentence(10).   % end if new line entered
end_of_sentence(0'.).
end_of_sentence(0'!).
end_of_sentence(0'?).
