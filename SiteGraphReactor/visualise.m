function D = visualise(path)
    rand('seed',100);
    if nargin==0
       path = './SiteGraphLTS/bin/Debug/'; 
    end
    f = fopen([path 'results.dat'], 'r');
    D = textscan(f,'%d\t%f\t%f\t%s','Delimiter','\n');
    fclose(f);
    X = D{2};
    Y = D{3};
    U = unique(D{4});
    C = rand(3,1);
    
    for i=1:length(U)
       idx = find(strcmp(D{4},U{i}));
       %nicesubplot(length(U),i);
       C = rand(3,1); hold on;
       plot(X(idx),Y(idx),'-','linewidth',2,'color',C);
       xlim([0,max(X)]);
       title(U{i});
    end
end

function h = nicesubplot(N, i, prefercols)
    M = ceil(sqrt(N));
    R = M;
    C = M;
    
    if N < 4
        R = N;
        C = 1;
    else
        while 1
            e = (R*C) - N;
            if (e < 0)
                R = R - 1;
                C = C + 1;
                break;
            else
                R = R + 1;
                C = C - 1;
            end
        end
    end
    
    
    if nargin == 3 && prefercols
        T = R;
        R = C;
        C = T;
    end
    h = subplot(R,C,i); 
    hold on;
end