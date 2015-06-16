%% Check stats chosen
nstats=16
nclass=4 
a=load('../data/input_data_test.txt');
for i=1:nstats
	figure(i)
	for j=1:nclass
		pos=find(a(:,1)==j-1);
		
		subplot(1,nclass,j)
		hist(a(pos,i+1))
	end
end

%% Leave most significant ones at first sight

%n=[2,3]
%b=a;
%a(:,n)=[];

%save ../data/save/input_data_chosen_test.txt a -ascii
