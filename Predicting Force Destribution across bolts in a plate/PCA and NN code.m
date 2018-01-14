clear all
cd '/Users/msahil515/IE 2065-Machine learning/Project/data'
pred=dlmread('_predictors.csv',',',1,1);
resp=dlmread('_responses.csv',',',1,1);
resp=resp(:,1:104);
pred(:,105)=zeros();


for i=1:length(pred)
    for j=1:104
        if pred(i,j)==0
            pred(i,105)=pred(i,105)+1;
        end
    end
end
ind_15=find(pred(:,105)<16);
pred_15=pred(ind_15,:);
resp_15=resp(ind_15,:);
duplicates=find(pred_15(:,105)==0);
duplicates(1)=[];
pred_15(duplicates,:)=[];
resp_15(duplicates,:)=[];
a=1:104;
[Q,R]=quorem(sym(a),8);
b=0:8:104;
b(1)=[];
Q(b)=Q(b)-1;
R(b)=R(b)+8;
Q=Q+1;
Q=double(Q);
R=double(R);


% creating an input matrix with 208 columns

for i=1:length(pred_15)
    for j=1:104
        if pred_15(i,j)==0
           
            inp(i,2*j-1)=R(j);
            inp(i,2*j)=Q(j);
        else
            inp(i,2*j-1)=0;
            inp(i,2*j)=0;
            
        end
        
    end
   
end


% % recoding the output matrix by giving a large force values to the failure
% % locations
 for i=1:length(resp_15)
     for j=1:104
         if resp(i,j)<0.1
             response(i,j)=500;
         else
             response(i,j)=resp(i,j);
         end
     end
 end
 
     
 
 
 
 [coeff,score,latent] = pca(resp_15)

[pcresp.coeff, pcresp.score, pcresp.latent, pcresp.tsquared, pcresp.explained] = pca(response);

pcresp.explained=cumsum(pcresp.explained);

%% Correlation Plots
% zPRED = zscore(pcaPRED.original);
% cPRED = (zPRED'*zPRED) / (numSamples-1);
% figure; contourf(cPRED), colorbar, title('Correlation between Predictor Variables');
% 
zRESP = zscore(resp_15);
cRESP = (zRESP'*zRESP) / (length(inp)-1);
figure; contourf(cRESP), colorbar, title('Correlation between Response Variables');

clear cPRED cRESP zPRED zRESP pcaPRED

%% PCA
[pcaRESP.coeff, pcaRESP.score, pcaRESP.latent, pcaRESP.tsquared, pcaRESP.explained] = pca(resp_15);

pcaRESP.explained=cumsum(pcaRESP.explained);
figure; bar(pcaRESP.explained), title('Cumulative Percent of Variance Explained'), xlabel('Score'), ylabel('Percent'), xlim([0,100]);

NNtargetPCA = pcaRESP.score(:,1:58);
[residuals,reconstructed]=pcares(resp_15,58);

% predictedScores = net(predictors');
% predictedScores = predictedScores';
% predicted = predictedScores * pcaRESP.coeff(1:32,:);
% 
% boltLoc = 84;
% figure; hold on
% scatter(responses(:,boltLoc),predicted(:,boltLoc) + mean(responses(:,boltLoc)));
% title('Observed vs. Predicted for One Selected Bolt Location');
% xlabel('Observed');
% ylabel('Predicted (PCA+ANN)');
% xlim([0,15]);
% ylim([0,15]);
% hold off




