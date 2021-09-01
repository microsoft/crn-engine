function sol = bme_export()

tfinal = 3000; % Final time for simulation
species = {'sp_0','sp_1','sp_2','sp4','sp5','sp6','sp7','sp8','sp9','sp10','sp11','sp12','sp13','sp14'}; % The list of all species
n = length(species);

% Write out the parameters

% Assign initial conditions
x0 = zeros(n,1);
x0(1) = 13.0;		% sp_0
x0(2) = 10.0;		% sp_1
x0(3) = 10.0;		% sp_2

% Solve the ODEs
[t,x] = ode15s(@odes,[0 tfinal],x0,[]);

% Write out a solution structure to be returned by the function
for i = 1:n
  sol.(species{i}) = x(:,i);
end

% Produce a plot
figure;
plot(t, x)
legend({'<2 3^ 4>', '<4 5^>', '<1>[2]:<6>[3^ 4]{5^*}', '<1>[2]:<6>[3^ 4]:<4>[5^]', '<1>[2]:<6>[3^]<4>:[4 5^]', '<1>[2]{3^*}:[4 5^]', '<1>[2]:<2>[3^]<4>:[4 5^]', '[2 3^]<4>:[4 5^]', '[2 3^ 4]:<4>[5^]', '[2 3^ 4]{5^*}', '<1 2>', '<1>[2]:<2>[3^ 4]:<4>[5^]', '<1>[2]:<2>[3^ 4]{5^*}', '<6 3^ 4>'})

return

%%%

function dxdt = odes(t,x)

% Write out the parameters

% Assign states
sp_0 = x(1);
sp_1 = x(2);
sp_2 = x(3);
sp4 = x(4);
sp5 = x(5);
sp6 = x(6);
sp7 = x(7);
sp8 = x(8);
sp9 = x(9);
sp10 = x(10);
sp11 = x(11);
sp12 = x(12);
sp13 = x(13);
sp14 = x(14);

% Define reaction propensities
r_0 = ((0.00065 * sp_1) * sp_2);
r_1 = (0.004 * sp4);
r_2 = ((8000.0 / 400.0) * sp4);
r_3 = (0.04 * sp5);
r_4 = ((8000.0 / 400.0) * sp5);
r_5 = ((0.00042 * sp_0) * sp6);
r_6 = (0.04 * sp7);
r_7 = ((8000.0 / 400.0) * sp7);
r_8 = ((8000.0 / 400.0) * sp7);
r_9 = ((8000.0 / 400.0) * sp8);
r_10 = (0.004 * sp9);
r_11 = ((8000.0 / 400.0) * sp9);
r_12 = ((0.00065 * sp_1) * sp10);
r_13 = (0.004 * sp12);
r_14 = ((8000.0 / 400.0) * sp12);
r_15 = ((8000.0 / 400.0) * sp12);
r_16 = ((0.00065 * sp_1) * sp13);
r_17 = ((8000.0 / 400.0) * sp13);
r_18 = ((0.00042 * sp14) * sp6);

% Assign derivatives
dsp_0 = -r_5 + r_6;
dsp_1 = -r_0 + r_1 + r_10 - r_12 + r_13 - r_16;
dsp_2 = -r_0 + r_1;
dsp4 = r_0 - r_1 - r_2 + r_4;
dsp5 = r_2 - r_3 - r_4 + r_18;
dsp6 = r_3 - r_5 + r_6 - r_18;
dsp7 = r_5 - r_6 - r_7 - r_8 + r_15;
dsp8 = r_7 - r_9 + r_11;
dsp9 = r_9 - r_10 - r_11 + r_12 + r_14;
dsp10 = r_10 - r_12 + r_17;
dsp11 = r_7 + r_14 + r_17;
dsp12 = r_8 - r_13 - r_14 - r_15 + r_16;
dsp13 = r_13 - r_16 - r_17;
dsp14 = r_3 - r_18;

dxdt = [dsp_0; dsp_1; dsp_2; dsp4; dsp5; dsp6; dsp7; dsp8; dsp9; dsp10; dsp11; dsp12; dsp13; dsp14];

return
