%https://www.mathworks.com/help/images/examples/correcting-nonuniform-illumination.html

cd('/media/work/projects/green-edge/');

clc;
clear;
close all;

I = imread('test.jpg');

%imshow(I);

background = imopen(I,strel('disk',300));

I2 = I - background;
imshow(I2)

I3 = imadjust(I2);
imshow(I3);

bw = imbinarize(I3);
bw = bwareaopen(bw, 5);
imshow(bw)

load('data/ice-cam/Calibration_coefficient_camera360_20160821.mat');

% Original image

II = I;
Cal.distance_max = 250;
II = flipdim(II,1);
II = flipdim(II,2);

f=find(abs(Val_dist-Cal.distance_max)<20);
II=II(1:f(1),:);
x_pos_cut=-x_pos(1:f(1),:);
y_pos_cut=-y_pos(1:f(1),:);

F=figure('Visible','on');
set(gca, 'color', [0 0 0]); hold on;
h=pcolor(x_pos_cut,y_pos_cut,double(II));
%caxis([0 256])
set(h, 'EdgeColor', 'none');
colormap('gray')
axis('image')
xlabel('Longitudinal (m)')
ylabel('Transverse (m)')

print('graphs/ice_cover1','-dpng')

% Image with ice detection

II = bw;
Cal.distance_max = 250;
II = flipdim(II,1);
II = flipdim(II,2);

f=find(abs(Val_dist-Cal.distance_max)<20);
II=II(1:f(1),:);
x_pos_cut=-x_pos(1:f(1),:);
y_pos_cut=-y_pos(1:f(1),:);

F=figure('Visible','on');
% set(gca, 'color', [0 0 0]); hold on;
h=pcolor(x_pos_cut,y_pos_cut,double(II));
%caxis([0 256])
set(h, 'EdgeColor', 'none');
colormap([0     0     0; 0.1686275 0.5490196 0.745098])
axis('image')
xlabel('Longitudinal (m)')
ylabel('Transverse (m)')

print('graphs/ice_cover2','-dpng')

% cc = bwconncomp(bw, 4);
% labeled = labelmatrix(cc);
% RGB_label = label2rgb(labeled, @spring, 'c', 'shuffle');
% imshow(RGB_label)