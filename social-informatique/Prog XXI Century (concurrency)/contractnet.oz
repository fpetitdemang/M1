%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Contract net protocol

% Author: Peter Van Roy
% Date: Nov. 5, 2003

% Given a user and a set of potential providers.  The user first sends the
% message query(price X) to each provider; each provider then sends its price
% back to the user (by binding X).  The user then sends the message 'ok' to the
% provider with the lowest price and the message 'no' to the others.

% This is a multi-agent program implemented with message-passing concurrency.
% The providers are port objects (a simple kind of active object).  The contract
% net protocol is implemented by using functional building blocks (Map, FoldL,
% ForAll) as concurrency patterns.  Because of the dataflow behavior, these
% building blocks work correctly in a concurrent setting.

%%%%%% Initialization %%%%%%
% Create a set of providers for the demonstration

declare
Providers=[_ _ _ _] % Four providers
for P in Providers I in 1..{Length Providers} do S in
   P={NewPort S}
   thread
      for M in S do
         case M of query(price P) then P=I*I-4*I
         [] ok then {Inspect ok(I)}
         [] no then {Inspect no(I)} end
      end
   end
end

%%%%%% Contract net protocol %%%%%%
% After execution, Pb is the provider with the lowest reply Rb

PRs={Map Providers fun {$ P} P#{Send P query(price $)} end}
Pb#Rb={FoldL PRs.2 fun {$ Pb#Rb P#R} if R<Rb then P#R else Pb#Rb end end PRs.1}
for P#R in PRs do {Send P if P==Pb then ok else no end} end

%%%%%% Explanation %%%%%%

% - Map sends the query and collects the prices
% - FoldL calculates the lowest price
% - ForAll (for) sends the final replies
% Note that the contract net could also be implemented with custom communications
% operations, such as a broadcast primitive.  What is special about our
% implementation is that it uses common building blocks from functional
% programming, which are usually used sequentially.

% Exercises for the reader:
% 1. Write a refined version of this protocol that handles the case when a
%    provider does not reply or only replies after a long time.  Hint: make a
%    version of Send called 'SafeSend' that encapsulates a time-out.
% 2. Write a refined version of this protocol that reduces latency by sending
%    a 'no' message whenever a price is received that is higher than another
%    received price.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
