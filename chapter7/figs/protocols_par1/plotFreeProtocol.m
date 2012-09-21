M=protocol.M;
N=protocol.N;

GAMMA = 2.675987E8;
bs=GAMMA.^2.*protocol.uG.^2.*protocol.usmalldel.^2.*(protocol.udelta-protocol.usmalldel/3);

    
    
    if isfield(protocol,'testrategy') && strcmp(protocol.testrategy,'variable')
        TE = protocol.udelta+protocol.usmalldel + opt_settings.Tprep,opt_settings.Tread
    else
        TE = max(protocol.delta+protocol.smalldel) + 2 * max(opt_settings.Tprep,opt_settings.Tread);
        TE = repmat(TE,size(protocol.delta));
    end
    
    if strcmp(protocol.schemetype,'asymptotic')
        [junk,order]=sort(bs(2:protocol.M));
        order=[1 order+1]
    else
        [junk,order]=sort(bs(1:protocol.M));        
    end
    
for i=order
    w=1;
    te=TE(i);
    fprintf('\\begin{minipage}{\\textwidth}\n');
    fprintf('  \\centering\n');
    fprintf('  \\raisebox{1cm}{\\sequence{%1.3f}{%1.3f}{%1.3f}{%1.3f}{%1.3f}}\\hspace{1cm}\n', protocol.udelta(i),protocol.usmalldel(i), protocol.uG(i),w,te);
    fprintf('  \\begin{tikzpicture}\n');
    fprintf('        \\begin{axis}[ xlabel=$x$, ylabel=$y$, zlabel=$z$,xmax=1, xmin=-1, ymax=1, ymin=-1, zmax=1, zmin=0, width=3.0cm, height=3.0cm, grid=major]\n');
    fprintf('        \\addplot3+[only marks] table {M%03d.txt};\n',i);
    fprintf('        \\end{axis}\n');
    fprintf('  \\end{tikzpicture}\n');
    fprintf('\\end{minipage}\n');
    
    subplot(2,4,i);
    r=[1:N]+(i-1)*N;
    scatter3(protocol.grad_dirs(r,1),protocol.grad_dirs(r,2),protocol.grad_dirs(r,3));xlim([-1,1]);ylim([-1,1]);zlim([-1,1])
    %fprintf('%1.2f %1.2f %1.2f %03.2f\n',protocol.uG(i), protocol.usmalldel(i), protocol.udelta(i), bs(i)/1e6);
    
    file=sprintf('M%03d.txt',i);
    fid=fopen(file,'w');
    fprintf(fid,'%1.2f %1.2f %1.2f\n',[protocol.grad_dirs(r,1),protocol.grad_dirs(r,2),protocol.grad_dirs(r,3)]');    
    fclose(fid);
end


