beta = linspace(-10,10,1000);

f = beta.*(1+beta.^2).^(-1);
plot(beta,f)
set(gca,'fontsize',14)
xlabel({'$\beta$'}, 'Interpreter','latex')
% ylabel({'$\rho_1$'},Interpreter,'latex')
ylabel({'$\rho_1$'}, 'Interpreter','latex')
axis tight

hgexport(gcf,'ex_2_4.jpg', hgexport('factorystyle'), 'Format', 'jpeg');
