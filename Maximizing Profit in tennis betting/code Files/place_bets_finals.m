function [ bets,money] = place_bets_f( money,diff,prob_reward,matches)

bets=zeros(size(matches));

high_bet = 999;
medium_bet =money*0.7;
low_bet=money*0.2;
for i=1:size(matches,1) 
    if diff(i)>diff(i+size(matches,1)) 
       if prob_reward(i) < 0.4 
           if  money >= low_bet
               bets(i)=low_bet;
           else bets(i)=money;
           end
       elseif prob_reward(i)>=0.4 && prob_reward(i)<=0.7
           if  money >=medium_bet
               bets(i) = medium_bet;
           else bets(i) = money;
           end
       elseif prob_reward(i) > 0.7 
           if money >=high_bet 
           bets(i)=high_bet;
           else bets(i)=money;
           end
       end
    else
        if prob_reward(i+size(matches,1)) < 0.4
            if money>=low_bet
           bets(i+size(matches,1))=low_bet;
            else bets(i+size(matches,1))=money;
            end
       elseif prob_reward(i+size(matches,1))>=0.4 && prob_reward(i)<=0.7
           if money >=medium_bet
               bets((i+size(matches,1))) = medium_bet;
           else bets((i+size(matches,1))) = money;
           end
       elseif prob_reward(i+size(matches,1)) > 0.7
           if money >=high_bet
               bets(i+size(matches,1))=high_bet;
           else bets(i+size(matches,1))=money;
           end
        end
    end
    money=money-bets(i);
end
end