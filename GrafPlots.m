clc
clear
clf
close all

load Portifolios.dat;
load retornoD.dat;
load distribuicao.dat;

load ITUB3LAST.txt;
load BBAS3LAST.txt;
load PETR4LAST.txt;
load VALE3LAST.txt;
load MGLU3LAST.txt;
load SUZB3LAST.txt;


sumITUB = sum(distribuicao(:,2))
sumBBAS = sum(distribuicao(:,3))
sumPETR = sum(distribuicao(:,4))
sumVALE = sum(distribuicao(:,5))
sumMGLU = sum(distribuicao(:,6))
sumSUZB = sum(distribuicao(:,7))

xaxi = [1:1:1512];
plot(xaxi,ITUB3LAST(1:1512,4),'linewidth',1,'k');
set(gca, 'FontSize', 15);
hold on
plot(xaxi,BBAS3LAST(1:1512,4),'linewidth',1,'b');
hold on
plot(xaxi,PETR4LAST(1:1512,4),'linewidth',1,'r');
hold on
plot(xaxi,VALE3LAST(1:1512,4),'linewidth',1,'g');
hold on
plot(xaxi,MGLU3LAST(1:1512,4),'linewidth',1,'y');
hold on
plot(xaxi,SUZB3LAST(1:1512,4),'linewidth',1,'c');
legend('ITUB3','BBAS3','PETR4','VALE3','MGLU3','SUZB3','location','northwest')
title('Ativos de 01/02/2016 à 11/03/2022')
xlabel('dias');ylabel('preço')
axis([0,1550])

figure



subplot(231)
plot(distribuicao(:,1),distribuicao(:,2),'k')
set(gca, 'FontSize', 15);
axis([-0.1,0.1,0,250])
title('ITUB3')
ylabel('nº de dias')
grid
subplot(232)
plot(distribuicao(:,1),distribuicao(:,3),'k')
set(gca, 'FontSize', 15);
axis([-0.1,0.1,0,250])
title('BBAS3')
grid
subplot(233)
plot(distribuicao(:,1),distribuicao(:,4),'k')
set(gca, 'FontSize', 15);
axis([-0.1,0.1,0,250])
title('PETR4')
grid
subplot(234)
plot(distribuicao(:,1),distribuicao(:,5),'k')
set(gca, 'FontSize', 15);
axis([-0.1,0.1,0,250])
title('VALE3')
ylabel('nº de dias')
xlabel('retorno')
grid
subplot(235)
plot(distribuicao(:,1),distribuicao(:,6),'k')
set(gca, 'FontSize', 15);
axis([-0.1,0.1,0,250])
title('MGLU3')
xlabel('retorno')
grid
subplot(236)
plot(distribuicao(:,1),distribuicao(:,7),'k')
set(gca, 'FontSize', 15);
axis([-0.1,0.1,0,250])
title('SUZB3')
xlabel('retorno')
grid

figure

xlabber = [1:1:1511];
subplot(231)
plot(xlabber,retornoD(:,1),'k')
set(gca, 'FontSize', 15);
axis([0,1512,-0.2,0.2])
title('ITUB3')
ylabel('retorno')
grid
subplot(232)
plot(xlabber,retornoD(:,2),'k')
set(gca, 'FontSize', 15);
axis([0,1512,-0.2,0.2])
title('BBAS3')
grid
subplot(233)
plot(xlabber,retornoD(:,3),'k')
set(gca, 'FontSize', 15);
axis([0,1512,-0.2,0.2])
title('PETR4')
grid
subplot(234)
plot(xlabber,retornoD(:,4),'k')
set(gca, 'FontSize', 15);
axis([0,1512,-0.2,0.2])
title('VALE3')
ylabel('retorno')
xlabel('dias')
grid
subplot(235)
plot(xlabber,retornoD(:,5),'k')
set(gca, 'FontSize', 15);
axis([0,1512,-0.2,0.2])
title('MGLU3')
xlabel('dias')
grid
subplot(236)
plot(xlabber,retornoD(:,6),'k')
set(gca, 'FontSize', 15);
axis([0,1512,-0.2,0.2])
title('SUZB3')
xlabel('dias')
grid



figure


maior = -1000;
indMa = 0;
menor = 1000;
indMe = 0;
ativos = 6
for i = 1:length(Portifolios)
  
if(Portifolios(i,ativos+4) > maior)
maior = Portifolios(i,ativos+4);
indMa = i;
endif  

if(Portifolios(i,ativos+3) < menor)
menor = Portifolios(i,ativos+3);
indMe = i;
endif  

endfor

MaiorSharpe = indMa
MenorRisco = indMe

% FRONTEIRA DE EFICIENCIA
FE(1,1) = Portifolios(indMe,ativos+3);
FE(2,1) = Portifolios(indMe,ativos+2);

indasda = 1;
for i = Portifolios(indMe,ativos+2)+0.00:0.01:1.01

lrisk = 100;
for j = 1:length(Portifolios)
if((Portifolios(j,ativos+2) >= i && Portifolios(j,ativos+2) < i+0.01) && (Portifolios(j,ativos+3) < lrisk))
lrisk = Portifolios(j,ativos+3);
endif  
endfor  

if(lrisk < 100)
indasda = indasda +1;
FE(1,indasda) = lrisk; 
FE(2,indasda) = i; 
endif

endfor


scatter(Portifolios(:,ativos+3),Portifolios(:,ativos+2),[],Portifolios(:,ativos+4),'filled')%plot(Portifolios(:,ativos+3),Portifolios(:,ativos+2),'d','linewidth',2)
colorbar("ylabel","índice Sharpe","FontSize",15)
colormap cool
set(gca, 'FontSize', 15);
hold on
plot(Portifolios(1,ativos+3),Portifolios(1,ativos+2),'sy','linewidth',5)
text(Portifolios(1,ativos+3)-0.075,Portifolios(1,ativos+2),'\bf % Iguais','color','black','FontSize', 15)
hold on

plot(Portifolios(2,ativos+3),Portifolios(2,ativos+2),'sc','linewidth',5)
text(Portifolios(2,ativos+3)+0.015,Portifolios(2,ativos+2),'\bf ITUB3','color','black','FontSize', 15)
hold on

plot(Portifolios(3,ativos+3),Portifolios(3,ativos+2),'sc','linewidth',5)
text(Portifolios(3,ativos+3)+0.015,Portifolios(3,ativos+2),'\bf BBAS3','color','black','FontSize', 15)
hold on

plot(Portifolios(4,ativos+3),Portifolios(4,ativos+2),'sc','linewidth',5)
text(Portifolios(4,ativos+3)+0.015,Portifolios(4,ativos+2),'\bf PETR4','color','black','FontSize', 15)
hold on

plot(Portifolios(5,ativos+3),Portifolios(5,ativos+2),'sc','linewidth',5)
text(Portifolios(5,ativos+3)+0.015,Portifolios(5,ativos+2),'\bf VALE3','color','black','FontSize', 15)
hold on

plot(Portifolios(6,ativos+3),Portifolios(6,ativos+2),'sc','linewidth',5)
text(Portifolios(6,ativos+3)+0.015,Portifolios(6,ativos+2),'\bf MGLU3','color','black','FontSize', 15)
hold on

plot(Portifolios(7,ativos+3),Portifolios(7,ativos+2),'sc','linewidth',5)
text(Portifolios(7,ativos+3)+0.015,Portifolios(7,ativos+2),'\bf SUZB3','color','black','FontSize', 15)
hold on

plot(Portifolios(indMa,ativos+3),Portifolios(indMa,ativos+2),'sg','linewidth',5)
text(Portifolios(indMa,ativos+3)-0.085,Portifolios(indMa,ativos+2),'\bf Max. Sharpe','color','black','FontSize', 15)
hold on
plot(Portifolios(indMe,ativos+3),Portifolios(indMe,ativos+2),'sr','linewidth',5)
text(Portifolios(indMe,ativos+3)-0.065,Portifolios(indMe,ativos+2),'\bf Min. Risco','color','black','FontSize', 15)
grid

hold on

plot(FE(1,:),FE(2,:),'linewidth',2,'r')
text(FE(1,length(FE)/2)+0.08,FE(2,length(FE)/2)+0.25,'\bf Fronteira Eficiente','color','black','FontSize', 15)

axis([0.15,0.75])
xlabel('risco');
ylabel('retorno');
%legend('potifólios')%'% iguais','Max. Sharpe','Min. Risco');
