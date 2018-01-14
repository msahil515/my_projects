prob=[01, .20, .29, .32, .21,  .4, .55, .14;
     .80,   1, .62, .65, .56, .59, .75, .42;
     .71, .38,   1, .51, .44, .48, .65, .11;
     .68, .35, .49,   1, .42, .43, .64, .22;
     .79, .44, .56, .58,   1, .55, .72, .36;
      .6, .41, .52, .57, .45,   1, .71, .31;
     .45, .25, .35, .36, .28, .29,   1, .02;
     .86, .58, .89, .78, .64, .69, .98,   1];
matches_q=[1,5;2,6;3,7;4,8];
matches_s=zeros(2,2);
matches_f=zeros(1,2);
championships=zeros(8,1);
names={'Querrey','Federer','Berdych','Tsonga','Roanic','Cilic','Pouille','Murray'};

reward_q=zeros(4,2);
reward_s=zeros(2,2);
reward_f=zeros(1,2);
error=.1;
rake=.95;
bet_q=zeros(4,2);
bet_s=zeros(2,2);
bet_f=zeros(1,2);
start_balance=1000;

sims=10000;
champion = 0;

results = zeros(1,sims);
%%%% Run simluation

for j=1:sims
money=start_balance;
    %%%% Generate Quarterfinal Odds
   prob_q = zeros(4,2);
    for i=1:4
        reward_q(i)=(1/prob(matches_q(i),matches_q(i+4)))*normrnd(1,error);
        prob_q(i) = prob(matches_q(i),matches_q(i+4));
    end
    for i=5:8
        reward_q(i)=1/(1-(1/reward_q(i-4)));
        prob_q(i) = prob(matches_q(i),matches_q(i-4));
    end
    reward_q=reward_q*rake;
    prob_reward_q = 1./reward_q;
    diff_q=prob_q-prob_reward_q;
    %%%%  Place bets quarterfinals
    
    [bet_q,money]=place_bets_q(money,diff_q,prob_reward_q,matches_q);
    

    %%%% Simulate Quarterfinal Matches
    rand_q=rand(4,1);  
    
    for m=1:4
    
        if rand_q(m)<=prob(matches_q(m,1),matches_q(m,2));
            matches_s(m)=matches_q(m,1);
        else
            matches_s(m)=matches_q(m,2);
        end
    end
    
matches_s=transpose(matches_s);
    
    %%%%  Collect winnings

    for i=1:2
        if matches_s(1,i) == matches_q(i,1) && bet_q(i,1) ~= 0
            money=money+bet_q(i,1)*reward_q(i,1);
        elseif matches_s(1,i) == matches_q(i,2) && bet_q(i,2) ~= 0
            money=money+bet_q(i,2)*reward_q(i,2);
        end
    end
    for i=1:2
        if matches_s(2,i) == matches_q(2+i,1) && bet_q(2+i,1) ~= 0
            money=money+bet_q(2+i,1)*reward_q(2+i,1);
        elseif matches_s(1,i) == matches_q(2+i,2) && bet_q(2+i,2) ~= 0
            money=money+bet_q(2+i,2)*reward_q(2+i,2);
        end
    end

    %%%% Generate Semifinal Odds
    prob_s = zeros(2,2);
    for i=1:2
        reward_s(i)=(1/prob(matches_s(i),matches_s(i+2)))*normrnd(1,error);
        prob_s(i) = prob(matches_s(i),matches_s(i+2));
    end
    for i=3:4
        reward_s(i)=1/(1-(1/reward_s(i-2)));
        prob_s(i) = prob(matches_s(i),matches_s(i-2));
    end
    reward_s=reward_s*rake;
    prob_reward_s = 1./reward_s;
    diff_s = prob_s - prob_reward_s;
  
        
    
    
    %%%%  Place bets semifinals
    
    [bet_s,money]=place_bets_s(money,diff_s,prob_reward_s,matches_s);
    
    %%%% Simulate Semifinal Matches
    rand_s=rand(2,1);
    for n=1:2

        if rand_s(n)<=prob(matches_s(n,1),matches_s(n,2))
            matches_f(n)=matches_s(n,1);
        else
            matches_f(n)=matches_s(n,2);
        end
    end
    %%%% Collect Semifinal winnings
    
    if matches_f(1)==matches_s(1,1) && bet_s(1,1)~=0
        money=money+bet_s(1,1)*reward_s(1,1);
    elseif matches_f(1)==matches_s(1,2) && bet_s(1,2)~=0
        money=money+bet_s(1,2)*reward_s(1,2);
    end
    
    %%%% Generate Finals Odds
    reward_f(1)=(1/prob(matches_f(1),matches_f(2)))*normrnd(1,error);
    reward_f(2)=1/(1-1/(reward_f(1)));
    reward_f=reward_f*rake;
    prob_reward_f = 1./reward_f;
    prob_f(1) = prob(matches_f(1),matches_f(2));
    prob_f(2) = 1-prob_f(1);
    diff_f = prob_f-prob_reward_f;
    %%%%  Place bets finals
    
    [bet_f,money]=place_bets_f(money,diff_f,prob_reward_f,matches_f);
    
    %%%% Simulate Finals
    rand_f=rand();
    if  rand_f<=prob(matches_f(1),matches_f(2))
        champion=matches_f(1);
    else
        champion=matches_f(2);
    end
    
    %%%% Collect finals winnings
    
    if champion==matches_f(1,1) && bet_f(1,1) ~= 0
        money=money+bet_f(1,1)*reward_f(1,1);
    elseif champion==matches_f(1,2) && bet_f(1,2) ~= 0
        money=money+bet_f(1,2)*reward_f(1,2);
    end
    results(j)=money;
    championships(champion)=championships(champion)+1;
end

% %%%%%%%  Plot
% bar(championships);
% set(gca,'xticklabel',names)
wins=0;
for x=1:length(results)
    if results(x)>=start_balance
        wins=wins+1;
    end
end
histogram(results,60)
avg = mean(results)
sigma = std(results)