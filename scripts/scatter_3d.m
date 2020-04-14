% Plotting 3D
% March 17, 2020


clear all;
close all;

% which nodes are on omega1 & omega2
scen_sens = zeros(6,3);
scen_sens(:,1) = [0,7.83,8.04,7.54,10.00,8.32];
scen_sens(:,2) = [0,5.68,5.268,4.284,10,8.007];
scen_sens(:,3) = [0,0.146,0.555,0.544,9.011,10];

scen_colors = zeros(6,3);
scen_colors(1,:) = [0,0,0];
scen_colors(2,:) = [178/255,34/255,34/255];
scen_colors(3,:) = [30/255,144/255,1];
scen_colors(4,:) = [0,205,0];
scen_colors(5,:) = [1,165/255,0];
scen_colors(6,:) = [160/255,32/255,240/255];

figure(1);
scatter3(scen_sens(:,1),scen_sens(:,2),scen_sens(:,3),400,scen_colors,...
    'filled','MarkerEdgeColor','k');



