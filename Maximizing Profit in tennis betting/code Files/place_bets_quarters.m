
function [ bets,money] = place_bets_q( money,diff,prob_reward,matches)

bets=zeros(size(matches));

high_bet = 987;
medium_bet = money*0.25;
low_bet=money*0.05;
for i=1:size(matches,1) 
    if diff(i)>diff(i+size(matches,1)) 
       if prob_reward(i) < 0.4 && money >= low_bet
           bets(i)=low_bet;
       elseif prob_reward(i)>=0.4 && prob_reward(i)<=0.7 && prob_reward(i)<=0.7 && money >=medium_bet
               bets(i) = medium_bet;
       elseif prob_reward(i) > 0.7 && money >=high_bet 
           bets(i)=high_bet;
       end
    else
        if prob_reward(i+size(matches,1)) < 0.4 
           bets(i+size(matches,1))=low_bet;
       elseif prob_reward(i+size(matches,1))>=0.4 && prob_reward(i)<=0.7
               bets((i+size(matches,1))) = medium_bet;
       elseif prob_reward(i+size(matches,1)) > 0.7 && money >=high_bet
           bets(i+size(matches,1))=high_bet;
        end
    end
    money=money-bets(i);
end
end
% function [ bets,money] = place_bets_1( money,diff,prob_reward,matches)
% 
% bets=zeros(size(matches));
% 
% high_bet = 250;
% medium_bet = 150;
% low_bet=75;
% for i=1:size(matches,1)  
%     if diff(i)>diff(i+size(matches,1))
%        if prob_reward(i) < 0.5 && money >= low_bet
%            bets(i)=low_bet;
%        elseif prob_reward(i)>=0.5 && prob_reward(i)<=0.7 && money >=medium_bet
%                bets(i) = medium_bet;
%        elseif prob_reward(i) > 0.7 && money >=high_bet
%            bets(i)=high_bet;
%        end
%     else
%         if prob_reward(i+size(matches,1)) < 0.5 && money >= low_bet
%            bets(i)=low_bet;
%        elseif prob_reward(i+size(matches,1))>=0.5 && prob_reward(i)<=0.7 && money >=medium_bet
%                bets(i) = medium_bet;
%        elseif prob_reward(i+size(matches,1)) > 0.7 && money >=high_bet
%            bets(i)=high_bet;
%         end
%     end
%     money=money-bets(i);
% end
% end




% for i=size(matches,1)+1:size(matches,1)*2  
%     if reward(i)>=(1/prob(matches(i),i-size(matches,1))) && money>=flat_bet
%         bets(i)=flat_bet;
%     end
%     money=money-bets(i);
% end
% end