 flow<-data.frame(cbind(Date=1,Q=rep(0,         365 )))
 colnames(flow)<-c("Date","Q")
 conc<-data.frame(cbind(Date=1,Q=rep(0,          11 )))
 colnames(conc)<-c("Date","Conc")
 num_strata<-3
 bound<-data.frame(Date=rep(0,           2 ))
 strata<-data.frame(Date=rep(0,           3 ))
flow$Date[1] <- "2015-01-01"; flow$Q[1] <-3095.499
flow$Date[2] <- "2015-01-02"; flow$Q[2] <-3579.569
flow$Date[3] <- "2015-01-03"; flow$Q[3] <-4484.015
flow$Date[4] <- "2015-01-04"; flow$Q[4] <-4624.141
flow$Date[5] <- "2015-01-05"; flow$Q[5] <-4089.116
flow$Date[6] <- "2015-01-06"; flow$Q[6] <-3503.137
flow$Date[7] <- "2015-01-07"; flow$Q[7] <-3375.750
flow$Date[8] <- "2015-01-08"; flow$Q[8] <-3936.252
flow$Date[9] <- "2015-01-09"; flow$Q[9] <-4636.879
flow$Date[10] <- "2015-01-10"; flow$Q[10] <-5439.416
flow$Date[11] <- "2015-01-11"; flow$Q[11] <-4904.392
flow$Date[12] <- "2015-01-12"; flow$Q[12] <-4713.312
flow$Date[13] <- "2015-01-13"; flow$Q[13] <-4878.914
flow$Date[14] <- "2015-01-14"; flow$Q[14] <-4904.392
flow$Date[15] <- "2015-01-15"; flow$Q[15] <-4815.221
flow$Date[16] <- "2015-01-16"; flow$Q[16] <-4394.845
flow$Date[17] <- "2015-01-17"; flow$Q[17] <-4063.639
flow$Date[18] <- "2015-01-18"; flow$Q[18] <-3999.945
flow$Date[19] <- "2015-01-19"; flow$Q[19] <-4114.594
flow$Date[20] <- "2015-01-20"; flow$Q[20] <-4331.151
flow$Date[21] <- "2015-01-21"; flow$Q[21] <-4407.583
flow$Date[22] <- "2015-01-22"; flow$Q[22] <-4356.628
flow$Date[23] <- "2015-01-23"; flow$Q[23] <-4305.674
flow$Date[24] <- "2015-01-24"; flow$Q[24] <-4203.764
flow$Date[25] <- "2015-01-25"; flow$Q[25] <-3923.513
flow$Date[26] <- "2015-01-26"; flow$Q[26] <-4038.161
flow$Date[27] <- "2015-01-27"; flow$Q[27] <-3796.127
flow$Date[28] <- "2015-01-28"; flow$Q[28] <-3681.478
flow$Date[29] <- "2015-01-29"; flow$Q[29] <-3745.172
flow$Date[30] <- "2015-01-30"; flow$Q[30] <-3566.830
flow$Date[31] <- "2015-01-31"; flow$Q[31] <-3515.876
flow$Date[32] <- "2015-02-01"; flow$Q[32] <-3681.478
flow$Date[33] <- "2015-02-02"; flow$Q[33] <-3108.238
flow$Date[34] <- "2015-02-03"; flow$Q[34] <-2955.374
flow$Date[35] <- "2015-02-04"; flow$Q[35] <-3477.660
flow$Date[36] <- "2015-02-05"; flow$Q[36] <-3248.363
flow$Date[37] <- "2015-02-06"; flow$Q[37] <-3235.625
flow$Date[38] <- "2015-02-07"; flow$Q[38] <-3783.388
flow$Date[39] <- "2015-02-08"; flow$Q[39] <-3796.127
flow$Date[40] <- "2015-02-09"; flow$Q[40] <-3898.036
flow$Date[41] <- "2015-02-10"; flow$Q[41] <-3490.398
flow$Date[42] <- "2015-02-11"; flow$Q[42] <-3171.931
flow$Date[43] <- "2015-02-12"; flow$Q[43] <-3108.238
flow$Date[44] <- "2015-02-13"; flow$Q[44] <-3044.544
flow$Date[45] <- "2015-02-14"; flow$Q[45] <-3006.328
flow$Date[46] <- "2015-02-15"; flow$Q[46] <-2955.374
flow$Date[47] <- "2015-02-16"; flow$Q[47] <-2904.419
flow$Date[48] <- "2015-02-17"; flow$Q[48] <-3070.022
flow$Date[49] <- "2015-02-18"; flow$Q[49] <-2942.635
flow$Date[50] <- "2015-02-19"; flow$Q[50] <-3197.409
flow$Date[51] <- "2015-02-20"; flow$Q[51] <-3095.499
flow$Date[52] <- "2015-02-21"; flow$Q[52] <-2980.851
flow$Date[53] <- "2015-02-22"; flow$Q[53] <-3095.499
flow$Date[54] <- "2015-02-23"; flow$Q[54] <-2942.635
flow$Date[55] <- "2015-02-24"; flow$Q[55] <-3095.499
flow$Date[56] <- "2015-02-25"; flow$Q[56] <-2942.635
flow$Date[57] <- "2015-02-26"; flow$Q[57] <-2764.294
flow$Date[58] <- "2015-02-27"; flow$Q[58] <-2840.726
flow$Date[59] <- "2015-02-28"; flow$Q[59] <-2751.555
flow$Date[60] <- "2015-03-01"; flow$Q[60] <-2675.123
flow$Date[61] <- "2015-03-02"; flow$Q[61] <-2980.851
flow$Date[62] <- "2015-03-03"; flow$Q[62] <-3210.147
flow$Date[63] <- "2015-03-04"; flow$Q[63] <-3159.193
flow$Date[64] <- "2015-03-05"; flow$Q[64] <-3057.283
flow$Date[65] <- "2015-03-06"; flow$Q[65] <-2942.635
flow$Date[66] <- "2015-03-07"; flow$Q[66] <-3197.409
flow$Date[67] <- "2015-03-08"; flow$Q[67] <-3312.057
flow$Date[68] <- "2015-03-09"; flow$Q[68] <-3643.262
flow$Date[69] <- "2015-03-10"; flow$Q[69] <-4076.378
flow$Date[70] <- "2015-03-11"; flow$Q[70] <-5261.075
flow$Date[71] <- "2015-03-12"; flow$Q[71] <-6649.591
flow$Date[72] <- "2015-03-13"; flow$Q[72] <-7031.751
flow$Date[73] <- "2015-03-14"; flow$Q[73] <-7286.525
flow$Date[74] <- "2015-03-15"; flow$Q[74] <-7452.128
flow$Date[75] <- "2015-03-16"; flow$Q[75] <-7630.469
flow$Date[76] <- "2015-03-17"; flow$Q[76] <-7401.173
flow$Date[77] <- "2015-03-18"; flow$Q[77] <-7477.605
flow$Date[78] <- "2015-03-19"; flow$Q[78] <-7159.138
flow$Date[79] <- "2015-03-20"; flow$Q[79] <-7057.229
flow$Date[80] <- "2015-03-21"; flow$Q[80] <-6700.546
flow$Date[81] <- "2015-03-22"; flow$Q[81] <-6445.772
flow$Date[82] <- "2015-03-23"; flow$Q[82] <-5961.702
flow$Date[83] <- "2015-03-24"; flow$Q[83] <-5783.361
flow$Date[84] <- "2015-03-25"; flow$Q[84] <-5515.848
flow$Date[85] <- "2015-03-26"; flow$Q[85] <-5439.416
flow$Date[86] <- "2015-03-27"; flow$Q[86] <-5362.984
flow$Date[87] <- "2015-03-28"; flow$Q[87] <-5019.040
flow$Date[88] <- "2015-03-29"; flow$Q[88] <-5108.211
flow$Date[89] <- "2015-03-30"; flow$Q[89] <-4292.935
flow$Date[90] <- "2015-03-31"; flow$Q[90] <-4815.221
flow$Date[91] <- "2015-04-01"; flow$Q[91] <-4471.277
flow$Date[92] <- "2015-04-02"; flow$Q[92] <-4458.538
flow$Date[93] <- "2015-04-03"; flow$Q[93] <-4560.447
flow$Date[94] <- "2015-04-04"; flow$Q[94] <-4407.583
flow$Date[95] <- "2015-04-05"; flow$Q[95] <-4356.628
flow$Date[96] <- "2015-04-06"; flow$Q[96] <-4394.845
flow$Date[97] <- "2015-04-07"; flow$Q[97] <-4101.855
flow$Date[98] <- "2015-04-08"; flow$Q[98] <-3872.559
flow$Date[99] <- "2015-04-09"; flow$Q[99] <-4280.196
flow$Date[100] <- "2015-04-10"; flow$Q[100] <-4038.161
flow$Date[101] <- "2015-04-11"; flow$Q[101] <-4382.106
flow$Date[102] <- "2015-04-12"; flow$Q[102] <-4152.810
flow$Date[103] <- "2015-04-13"; flow$Q[103] <-4382.106
flow$Date[104] <- "2015-04-14"; flow$Q[104] <-4471.277
flow$Date[105] <- "2015-04-15"; flow$Q[105] <-4229.242
flow$Date[106] <- "2015-04-16"; flow$Q[106] <-4229.242
flow$Date[107] <- "2015-04-17"; flow$Q[107] <-4407.583
flow$Date[108] <- "2015-04-18"; flow$Q[108] <-3948.991
flow$Date[109] <- "2015-04-19"; flow$Q[109] <-4114.594
flow$Date[110] <- "2015-04-20"; flow$Q[110] <-4560.447
flow$Date[111] <- "2015-04-21"; flow$Q[111] <-4815.221
flow$Date[112] <- "2015-04-22"; flow$Q[112] <-4917.130
flow$Date[113] <- "2015-04-23"; flow$Q[113] <-4955.346
flow$Date[114] <- "2015-04-24"; flow$Q[114] <-4891.653
flow$Date[115] <- "2015-04-25"; flow$Q[115] <-4827.960
flow$Date[116] <- "2015-04-26"; flow$Q[116] <-4751.528
flow$Date[117] <- "2015-04-27"; flow$Q[117] <-4331.151
flow$Date[118] <- "2015-04-28"; flow$Q[118] <-4241.980
flow$Date[119] <- "2015-04-29"; flow$Q[119] <-4241.980
flow$Date[120] <- "2015-04-30"; flow$Q[120] <-4191.026
flow$Date[121] <- "2015-05-01"; flow$Q[121] <-3694.217
flow$Date[122] <- "2015-05-02"; flow$Q[122] <-3617.785
flow$Date[123] <- "2015-05-03"; flow$Q[123] <-3541.353
flow$Date[124] <- "2015-05-04"; flow$Q[124] <-3630.524
flow$Date[125] <- "2015-05-05"; flow$Q[125] <-3719.694
flow$Date[126] <- "2015-05-06"; flow$Q[126] <-3885.297
flow$Date[127] <- "2015-05-07"; flow$Q[127] <-4038.161
flow$Date[128] <- "2015-05-08"; flow$Q[128] <-3694.217
flow$Date[129] <- "2015-05-09"; flow$Q[129] <-4127.332
flow$Date[130] <- "2015-05-10"; flow$Q[130] <-4203.764
flow$Date[131] <- "2015-05-11"; flow$Q[131] <-4433.061
flow$Date[132] <- "2015-05-12"; flow$Q[132] <-4598.663
flow$Date[133] <- "2015-05-13"; flow$Q[133] <-4687.834
flow$Date[134] <- "2015-05-14"; flow$Q[134] <-4280.196
flow$Date[135] <- "2015-05-15"; flow$Q[135] <-4292.935
flow$Date[136] <- "2015-05-16"; flow$Q[136] <-4140.071
flow$Date[137] <- "2015-05-17"; flow$Q[137] <-4050.900
flow$Date[138] <- "2015-05-18"; flow$Q[138] <-3974.468
flow$Date[139] <- "2015-05-19"; flow$Q[139] <-3503.137
flow$Date[140] <- "2015-05-20"; flow$Q[140] <-3719.694
flow$Date[141] <- "2015-05-21"; flow$Q[141] <-3337.534
flow$Date[142] <- "2015-05-22"; flow$Q[142] <-3401.227
flow$Date[143] <- "2015-05-23"; flow$Q[143] <-3171.931
flow$Date[144] <- "2015-05-24"; flow$Q[144] <-2891.680
flow$Date[145] <- "2015-05-25"; flow$Q[145] <-3031.806
flow$Date[146] <- "2015-05-26"; flow$Q[146] <-3031.806
flow$Date[147] <- "2015-05-27"; flow$Q[147] <-3146.454
flow$Date[148] <- "2015-05-28"; flow$Q[148] <-3235.625
flow$Date[149] <- "2015-05-29"; flow$Q[149] <-2942.635
flow$Date[150] <- "2015-05-30"; flow$Q[150] <-3070.022
flow$Date[151] <- "2015-05-31"; flow$Q[151] <-6573.159
flow$Date[152] <- "2015-06-01"; flow$Q[152] <-7286.525
flow$Date[153] <- "2015-06-02"; flow$Q[153] <-6292.908
flow$Date[154] <- "2015-06-03"; flow$Q[154] <-5847.054
flow$Date[155] <- "2015-06-04"; flow$Q[155] <-5452.155
flow$Date[156] <- "2015-06-05"; flow$Q[156] <-5057.256
flow$Date[157] <- "2015-06-06"; flow$Q[157] <-4853.437
flow$Date[158] <- "2015-06-07"; flow$Q[158] <-4407.583
flow$Date[159] <- "2015-06-08"; flow$Q[159] <-4598.663
flow$Date[160] <- "2015-06-09"; flow$Q[160] <-5350.245
flow$Date[161] <- "2015-06-10"; flow$Q[161] <-4942.608
flow$Date[162] <- "2015-06-11"; flow$Q[162] <-5159.165
flow$Date[163] <- "2015-06-12"; flow$Q[163] <-5630.496
flow$Date[164] <- "2015-06-13"; flow$Q[164] <-5095.472
flow$Date[165] <- "2015-06-14"; flow$Q[165] <-4993.562
flow$Date[166] <- "2015-06-15"; flow$Q[166] <-5210.120
flow$Date[167] <- "2015-06-16"; flow$Q[167] <-6636.852
flow$Date[168] <- "2015-06-17"; flow$Q[168] <-7872.504
flow$Date[169] <- "2015-06-18"; flow$Q[169] <-8369.313
flow$Date[170] <- "2015-06-19"; flow$Q[170] <-8776.950
flow$Date[171] <- "2015-06-20"; flow$Q[171] <-8509.438
flow$Date[172] <- "2015-06-21"; flow$Q[172] <-8152.755
flow$Date[173] <- "2015-06-22"; flow$Q[173] <-7668.685
flow$Date[174] <- "2015-06-23"; flow$Q[174] <-7120.922
flow$Date[175] <- "2015-06-24"; flow$Q[175] <-6700.546
flow$Date[176] <- "2015-06-25"; flow$Q[176] <-6483.988
flow$Date[177] <- "2015-06-26"; flow$Q[177] <-6216.476
flow$Date[178] <- "2015-06-27"; flow$Q[178] <-6802.455
flow$Date[179] <- "2015-06-28"; flow$Q[179] <-7375.696
flow$Date[180] <- "2015-06-29"; flow$Q[180] <-7184.615
flow$Date[181] <- "2015-06-30"; flow$Q[181] <-6687.807
flow$Date[182] <- "2015-07-01"; flow$Q[182] <-6853.410
flow$Date[183] <- "2015-07-02"; flow$Q[183] <-7031.751
flow$Date[184] <- "2015-07-03"; flow$Q[184] <-6675.068
flow$Date[185] <- "2015-07-04"; flow$Q[185] <-5974.441
flow$Date[186] <- "2015-07-05"; flow$Q[186] <-5936.225
flow$Date[187] <- "2015-07-06"; flow$Q[187] <-5847.054
flow$Date[188] <- "2015-07-07"; flow$Q[188] <-5439.416
flow$Date[189] <- "2015-07-08"; flow$Q[189] <-5388.462
flow$Date[190] <- "2015-07-09"; flow$Q[190] <-6547.681
flow$Date[191] <- "2015-07-10"; flow$Q[191] <-7668.685
flow$Date[192] <- "2015-07-11"; flow$Q[192] <-7681.424
flow$Date[193] <- "2015-07-12"; flow$Q[193] <-6891.626
flow$Date[194] <- "2015-07-13"; flow$Q[194] <-7031.751
flow$Date[195] <- "2015-07-14"; flow$Q[195] <-8764.212
flow$Date[196] <- "2015-07-15"; flow$Q[196] <-9031.724
flow$Date[197] <- "2015-07-16"; flow$Q[197] <-8929.815
flow$Date[198] <- "2015-07-17"; flow$Q[198] <-9044.463
flow$Date[199] <- "2015-07-18"; flow$Q[199] <-10751.446
flow$Date[200] <- "2015-07-19"; flow$Q[200] <-12509.384
flow$Date[201] <- "2015-07-20"; flow$Q[201] <-11553.983
flow$Date[202] <- "2015-07-21"; flow$Q[202] <-10267.376
flow$Date[203] <- "2015-07-22"; flow$Q[203] <-9541.271
flow$Date[204] <- "2015-07-23"; flow$Q[204] <-8522.177
flow$Date[205] <- "2015-07-24"; flow$Q[205] <-7923.459
flow$Date[206] <- "2015-07-25"; flow$Q[206] <-7299.264
flow$Date[207] <- "2015-07-26"; flow$Q[207] <-6751.500
flow$Date[208] <- "2015-07-27"; flow$Q[208] <-6624.113
flow$Date[209] <- "2015-07-28"; flow$Q[209] <-6267.430
flow$Date[210] <- "2015-07-29"; flow$Q[210] <-5643.235
flow$Date[211] <- "2015-07-30"; flow$Q[211] <-5171.904
flow$Date[212] <- "2015-07-31"; flow$Q[212] <-4980.824
flow$Date[213] <- "2015-08-01"; flow$Q[213] <-4624.141
flow$Date[214] <- "2015-08-02"; flow$Q[214] <-4178.287
flow$Date[215] <- "2015-08-03"; flow$Q[215] <-4980.824
flow$Date[216] <- "2015-08-04"; flow$Q[216] <-4636.879
flow$Date[217] <- "2015-08-05"; flow$Q[217] <-4496.754
flow$Date[218] <- "2015-08-06"; flow$Q[218] <-4331.151
flow$Date[219] <- "2015-08-07"; flow$Q[219] <-4241.980
flow$Date[220] <- "2015-08-08"; flow$Q[220] <-4127.332
flow$Date[221] <- "2015-08-09"; flow$Q[221] <-3757.911
flow$Date[222] <- "2015-08-10"; flow$Q[222] <-3541.353
flow$Date[223] <- "2015-08-11"; flow$Q[223] <-3681.478
flow$Date[224] <- "2015-08-12"; flow$Q[224] <-3503.137
flow$Date[225] <- "2015-08-13"; flow$Q[225] <-3273.841
flow$Date[226] <- "2015-08-14"; flow$Q[226] <-3133.715
flow$Date[227] <- "2015-08-15"; flow$Q[227] <-3273.841
flow$Date[228] <- "2015-08-16"; flow$Q[228] <-2955.374
flow$Date[229] <- "2015-08-17"; flow$Q[229] <-3019.067
flow$Date[230] <- "2015-08-18"; flow$Q[230] <-3579.569
flow$Date[231] <- "2015-08-19"; flow$Q[231] <-3413.966
flow$Date[232] <- "2015-08-20"; flow$Q[232] <-3312.057
flow$Date[233] <- "2015-08-21"; flow$Q[233] <-3375.750
flow$Date[234] <- "2015-08-22"; flow$Q[234] <-3108.238
flow$Date[235] <- "2015-08-23"; flow$Q[235] <-3273.841
flow$Date[236] <- "2015-08-24"; flow$Q[236] <-3286.579
flow$Date[237] <- "2015-08-25"; flow$Q[237] <-3286.579
flow$Date[238] <- "2015-08-26"; flow$Q[238] <-3108.238
flow$Date[239] <- "2015-08-27"; flow$Q[239] <-2993.590
flow$Date[240] <- "2015-08-28"; flow$Q[240] <-2904.419
flow$Date[241] <- "2015-08-29"; flow$Q[241] <-2827.987
flow$Date[242] <- "2015-08-30"; flow$Q[242] <-3019.067
flow$Date[243] <- "2015-08-31"; flow$Q[243] <-2891.680
flow$Date[244] <- "2015-09-01"; flow$Q[244] <-2853.464
flow$Date[245] <- "2015-09-02"; flow$Q[245] <-2777.032
flow$Date[246] <- "2015-09-03"; flow$Q[246] <-2764.294
flow$Date[247] <- "2015-09-04"; flow$Q[247] <-2726.077
flow$Date[248] <- "2015-09-05"; flow$Q[248] <-2662.384
flow$Date[249] <- "2015-09-06"; flow$Q[249] <-2445.827
flow$Date[250] <- "2015-09-07"; flow$Q[250] <-2509.520
flow$Date[251] <- "2015-09-08"; flow$Q[251] <-2624.168
flow$Date[252] <- "2015-09-09"; flow$Q[252] <-2624.168
flow$Date[253] <- "2015-09-10"; flow$Q[253] <-2687.861
flow$Date[254] <- "2015-09-11"; flow$Q[254] <-2917.158
flow$Date[255] <- "2015-09-12"; flow$Q[255] <-2751.555
flow$Date[256] <- "2015-09-13"; flow$Q[256] <-2662.384
flow$Date[257] <- "2015-09-14"; flow$Q[257] <-2624.168
flow$Date[258] <- "2015-09-15"; flow$Q[258] <-2407.610
flow$Date[259] <- "2015-09-16"; flow$Q[259] <-2496.781
flow$Date[260] <- "2015-09-17"; flow$Q[260] <-2407.610
flow$Date[261] <- "2015-09-18"; flow$Q[261] <-2815.248
flow$Date[262] <- "2015-09-19"; flow$Q[262] <-3961.729
flow$Date[263] <- "2015-09-20"; flow$Q[263] <-4114.594
flow$Date[264] <- "2015-09-21"; flow$Q[264] <-3350.273
flow$Date[265] <- "2015-09-22"; flow$Q[265] <-3197.409
flow$Date[266] <- "2015-09-23"; flow$Q[266] <-2751.555
flow$Date[267] <- "2015-09-24"; flow$Q[267] <-3006.328
flow$Date[268] <- "2015-09-25"; flow$Q[268] <-2942.635
flow$Date[269] <- "2015-09-26"; flow$Q[269] <-2904.419
flow$Date[270] <- "2015-09-27"; flow$Q[270] <-2713.339
flow$Date[271] <- "2015-09-28"; flow$Q[271] <-2662.384
flow$Date[272] <- "2015-09-29"; flow$Q[272] <-2687.861
flow$Date[273] <- "2015-09-30"; flow$Q[273] <-2929.896
flow$Date[274] <- "2015-10-01"; flow$Q[274] <-2636.907
flow$Date[275] <- "2015-10-02"; flow$Q[275] <-2598.691
flow$Date[276] <- "2015-10-03"; flow$Q[276] <-2343.917
flow$Date[277] <- "2015-10-04"; flow$Q[277] <-2420.349
flow$Date[278] <- "2015-10-05"; flow$Q[278] <-2471.304
flow$Date[279] <- "2015-10-06"; flow$Q[279] <-2433.088
flow$Date[280] <- "2015-10-07"; flow$Q[280] <-2356.656
flow$Date[281] <- "2015-10-08"; flow$Q[281] <-2292.962
flow$Date[282] <- "2015-10-09"; flow$Q[282] <-2433.088
flow$Date[283] <- "2015-10-10"; flow$Q[283] <-2331.178
flow$Date[284] <- "2015-10-11"; flow$Q[284] <-2280.224
flow$Date[285] <- "2015-10-12"; flow$Q[285] <-2318.440
flow$Date[286] <- "2015-10-13"; flow$Q[286] <-2203.792
flow$Date[287] <- "2015-10-14"; flow$Q[287] <-2305.701
flow$Date[288] <- "2015-10-15"; flow$Q[288] <-2216.530
flow$Date[289] <- "2015-10-16"; flow$Q[289] <-2331.178
flow$Date[290] <- "2015-10-17"; flow$Q[290] <-2191.053
flow$Date[291] <- "2015-10-18"; flow$Q[291] <-1999.973
flow$Date[292] <- "2015-10-19"; flow$Q[292] <-2292.962
flow$Date[293] <- "2015-10-20"; flow$Q[293] <-2114.621
flow$Date[294] <- "2015-10-21"; flow$Q[294] <-2471.304
flow$Date[295] <- "2015-10-22"; flow$Q[295] <-2191.053
flow$Date[296] <- "2015-10-23"; flow$Q[296] <-2101.882
flow$Date[297] <- "2015-10-24"; flow$Q[297] <-2242.008
flow$Date[298] <- "2015-10-25"; flow$Q[298] <-2025.450
flow$Date[299] <- "2015-10-26"; flow$Q[299] <-2433.088
flow$Date[300] <- "2015-10-27"; flow$Q[300] <-2369.394
flow$Date[301] <- "2015-10-28"; flow$Q[301] <-2458.565
flow$Date[302] <- "2015-10-29"; flow$Q[302] <-2649.645
flow$Date[303] <- "2015-10-30"; flow$Q[303] <-2636.907
flow$Date[304] <- "2015-10-31"; flow$Q[304] <-2407.610
flow$Date[305] <- "2015-11-01"; flow$Q[305] <-2904.419
flow$Date[306] <- "2015-11-02"; flow$Q[306] <-2700.600
flow$Date[307] <- "2015-11-03"; flow$Q[307] <-2394.872
flow$Date[308] <- "2015-11-04"; flow$Q[308] <-2636.907
flow$Date[309] <- "2015-11-05"; flow$Q[309] <-2598.691
flow$Date[310] <- "2015-11-06"; flow$Q[310] <-2751.555
flow$Date[311] <- "2015-11-07"; flow$Q[311] <-2636.907
flow$Date[312] <- "2015-11-08"; flow$Q[312] <-2458.565
flow$Date[313] <- "2015-11-09"; flow$Q[313] <-2522.259
flow$Date[314] <- "2015-11-10"; flow$Q[314] <-2509.520
flow$Date[315] <- "2015-11-11"; flow$Q[315] <-2547.736
flow$Date[316] <- "2015-11-12"; flow$Q[316] <-2585.952
flow$Date[317] <- "2015-11-13"; flow$Q[317] <-2369.394
flow$Date[318] <- "2015-11-14"; flow$Q[318] <-2662.384
flow$Date[319] <- "2015-11-15"; flow$Q[319] <-2675.123
flow$Date[320] <- "2015-11-16"; flow$Q[320] <-2420.349
flow$Date[321] <- "2015-11-17"; flow$Q[321] <-2356.656
flow$Date[322] <- "2015-11-18"; flow$Q[322] <-2547.736
flow$Date[323] <- "2015-11-19"; flow$Q[323] <-2738.816
flow$Date[324] <- "2015-11-20"; flow$Q[324] <-2382.133
flow$Date[325] <- "2015-11-21"; flow$Q[325] <-2534.997
flow$Date[326] <- "2015-11-22"; flow$Q[326] <-2585.952
flow$Date[327] <- "2015-11-23"; flow$Q[327] <-2636.907
flow$Date[328] <- "2015-11-24"; flow$Q[328] <-2598.691
flow$Date[329] <- "2015-11-25"; flow$Q[329] <-2840.726
flow$Date[330] <- "2015-11-26"; flow$Q[330] <-2394.872
flow$Date[331] <- "2015-11-27"; flow$Q[331] <-2968.112
flow$Date[332] <- "2015-11-28"; flow$Q[332] <-3490.398
flow$Date[333] <- "2015-11-29"; flow$Q[333] <-3413.966
flow$Date[334] <- "2015-11-30"; flow$Q[334] <-3401.227
flow$Date[335] <- "2015-12-01"; flow$Q[335] <-3732.433
flow$Date[336] <- "2015-12-02"; flow$Q[336] <-3554.092
flow$Date[337] <- "2015-12-03"; flow$Q[337] <-3363.011
flow$Date[338] <- "2015-12-04"; flow$Q[338] <-3515.876
flow$Date[339] <- "2015-12-05"; flow$Q[339] <-3006.328
flow$Date[340] <- "2015-12-06"; flow$Q[340] <-3031.806
flow$Date[341] <- "2015-12-07"; flow$Q[341] <-3095.499
flow$Date[342] <- "2015-12-08"; flow$Q[342] <-3261.102
flow$Date[343] <- "2015-12-09"; flow$Q[343] <-2929.896
flow$Date[344] <- "2015-12-10"; flow$Q[344] <-2917.158
flow$Date[345] <- "2015-12-11"; flow$Q[345] <-2726.077
flow$Date[346] <- "2015-12-12"; flow$Q[346] <-2929.896
flow$Date[347] <- "2015-12-13"; flow$Q[347] <-2942.635
flow$Date[348] <- "2015-12-14"; flow$Q[348] <-3120.977
flow$Date[349] <- "2015-12-15"; flow$Q[349] <-3146.454
flow$Date[350] <- "2015-12-16"; flow$Q[350] <-3273.841
flow$Date[351] <- "2015-12-17"; flow$Q[351] <-3095.499
flow$Date[352] <- "2015-12-18"; flow$Q[352] <-3057.283
flow$Date[353] <- "2015-12-19"; flow$Q[353] <-2815.248
flow$Date[354] <- "2015-12-20"; flow$Q[354] <-2917.158
flow$Date[355] <- "2015-12-21"; flow$Q[355] <-2917.158
flow$Date[356] <- "2015-12-22"; flow$Q[356] <-3375.750
flow$Date[357] <- "2015-12-23"; flow$Q[357] <-3898.036
flow$Date[358] <- "2015-12-24"; flow$Q[358] <-4777.005
flow$Date[359] <- "2015-12-25"; flow$Q[359] <-4560.447
flow$Date[360] <- "2015-12-26"; flow$Q[360] <-4305.674
flow$Date[361] <- "2015-12-27"; flow$Q[361] <-4636.879
flow$Date[362] <- "2015-12-28"; flow$Q[362] <-5273.813
flow$Date[363] <- "2015-12-29"; flow$Q[363] <-6815.194
flow$Date[364] <- "2015-12-30"; flow$Q[364] <-7401.173
flow$Date[365] <- "2015-12-31"; flow$Q[365] <-6445.772
conc$Date[1] <- "2015-04-14"; conc$Conc[1] <- 2.995
conc$Date[2] <- "2015-05-18"; conc$Conc[2] <- 3.845
conc$Date[3] <- "2015-06-18"; conc$Conc[3] <- 7.274
conc$Date[4] <- "2015-07-16"; conc$Conc[4] <- 1.880
conc$Date[5] <- "2015-07-30"; conc$Conc[5] <- 1.712
conc$Date[6] <- "2015-08-27"; conc$Conc[6] <- 2.318
conc$Date[7] <- "2015-09-14"; conc$Conc[7] <- 1.784
conc$Date[8] <- "2015-10-08"; conc$Conc[8] <- 3.177
conc$Date[9] <- "2015-10-22"; conc$Conc[9] <- 1.868
conc$Date[10] <- "2015-11-05"; conc$Conc[10] <- 1.374
conc$Date[11] <- "2015-11-19"; conc$Conc[11] <- 1.375
bound$Date[1]<-"2015-06-18"
bound$Date[2]<-"2015-10-12"
 dummy<-runif(length(flow$Date))*max(conc$Conc)*1.1
 flow$Date<-as.Date(flow$Date)
 conc$Date<-as.Date(conc$Date)
 bound$Date<-as.Date(bound$Date)
 first<-flow$Date[1]
 last<-flow$Date[length(flow$Date)]
 datetext<-paste(format(first,format="%B %Y"),"through",format(last,format="%B %Y"))
 strata$Date[1]<-mean(flow$Date[flow$Date<=bound$Date[1]])
 strata$Date[num_strata]<-mean(flow$Date[flow$Date>bound$Date[num_strata-1]])
strata$Date[2]<-mean(flow$Date[flow$Date>bound$Date[1] & flow$Date<=bound$Date[2]])
 png(filename = "output/St.JosephRiver__total_pcb_20150101-20151231.png", width = 1024, height = 768, units = "px", pointsize = 12, bg = "white", res = NA)
 nf <- layout(matrix(c(1,2),2,1,byrow=F), width=2,height=c(5,1))
 # set margins: bottom, left side, top, right side
 par(mar=c(3,6,5,6))
 par(bg="white")
 par(tcl=0.35)
 y<-flow$Q
 y[y>median(y)]<-y[y>median(y)]*1.1
 plot(x=flow$Date,y=y, type="n", axes=FALSE, ann=FALSE)
 mtext("DISCHARGE, IN CFS",side=2,line=4,font=2,cex=1.1)
 lines(x=flow$Date,y=flow$Q, col="steelblue")
 points(x=flow$Date,y=flow$Q, pch=19, col="steelblue", cex=0.4)
 axis(2, col.axis="blue", las=1)
 axis.Date(1,flow$Date,col.axis="blue",format="%b-%y",at=seq(as.Date(flow$Date[1]),as.Date(flow$Date[length(flow$Date)]),"months"))
 axis.Date(1,flow$Date,side=3,labels=F,at=seq(as.Date(flow$Date[1]),as.Date(flow$Date[length(flow$Date)]),"months"))
 title.txt<-paste("St.Joseph River:  total_pcb (",datetext,")")
 title(main=title.txt, font.main=2, col.main="red",line=3)
 mtext(expression("Load: 13.7 Kg" %+-% "12.0 Kg     RMSE: 2.8138 Kg     Annual Load: 13.7 Kg" %+-% "12.0 Kg     Annual Load (jackknife): 13.0 Kg" %+-% "11.8 Kg"),side=3,line=1,font=2,cex=0.9)
 par(new=TRUE)
 plot(x=flow$Date,y=dummy, type="n", axes=FALSE, ann=FALSE)
 axis(4, col.axis="blue", las=1)
 mtext(" TOTAL_PCB, IN NANOGRAMS PER LITER",side=4,line=4,font=2,cex=1.1)
 abline(v=conc$Date,col="grey80")
 points(x=conc$Date,y=conc$Conc, pch=22, bg="red", cex=1.2)
 if(mean(conc$Conc)<0.1) {
text(x=conc$Date+2,y=conc$Conc + 0.240, sprintf("%.3f",conc$Conc),cex=0.6,srt=90)
 } else {
text(x=conc$Date+2,y=conc$Conc + 0.240, sprintf("%.2f",conc$Conc),cex=0.6,srt=90)
 }
 abline(v=bound$Date+1.5,col="red",lty=3,lwd=2)
 text(x=strata$Date[1],y=0.97*max(dummy),"10.2 Kg",srt=90,cex=0.7,col="red")
 text(x=strata$Date[2],y=0.97*max(dummy),"2.6638 Kg",srt=90,cex=0.7,col="red")
 text(x=strata$Date[3],y=0.97*max(dummy),"0.88847 Kg",srt=90,cex=0.7,col="red")
 box()
 leg.txt<-c("DISCHARGE","CONCENTRATION","STRATA BOUNDARY")
 leg.col<-c("steelblue","black","red")
 leg.cex<-0.8
 leg.lty<-c(1,NA,3)
 leg.lwd<-c(1,NA,2)
 leg.pch<-c(19,22,NA)
 pt.bg<-c("black","red","black")
 par(mar=c(3,0,0,0))
 plot(1:10,1:10,type="n",axes=F,ann=F)
 mtext("Flow filename: St_Joseph_River_discharge_dv_2015-01-01_2015-12-31.txt      Concentration filename: St.Joseph_River__total_pcb__2015-01-01_2015-12-31.txt",side=1,line=1,font=1,cex=0.7)
 legend("center",legend=leg.txt,lty=leg.lty,col=leg.col,pch=leg.pch,lwd=leg.lwd,cex=leg.cex,pt.bg=pt.bg,inset=c(0.02,0.02),title="EXPLANATION")
 dev.off()
 dummy<-runif(length(flow$Date))*max(conc$Conc)*1.1
 flow$Date<-as.Date(flow$Date)
 conc$Date<-as.Date(conc$Date)
 bound$Date<-as.Date(bound$Date)
 first<-flow$Date[1]
 last<-flow$Date[length(flow$Date)]
 datetext<-paste(format(first,format="%B %Y"),"through",format(last,format="%B %Y"))
 strata$Date[1]<-mean(flow$Date[flow$Date<=bound$Date[1]])
 strata$Date[num_strata]<-mean(flow$Date[flow$Date>bound$Date[num_strata-1]])
strata$Date[2]<-mean(flow$Date[flow$Date>bound$Date[1] & flow$Date<=bound$Date[2]])
 pdf(file = "output/St.JosephRiver__total_pcb_20150101-20151231.pdf", width = 11, height = 8.5)
 nf <- layout(matrix(c(1,2),2,1,byrow=F), width=2,height=c(5,1))
 # set margins: bottom, left side, top, right side
 par(mar=c(3,6,5,6))
 par(bg="white")
 par(tcl=0.35)
 y<-flow$Q
 y[y>median(y)]<-y[y>median(y)]*1.1
 plot(x=flow$Date,y=y, type="n", axes=FALSE, ann=FALSE)
 mtext("DISCHARGE, IN CFS",side=2,line=4,font=2,cex=1.1)
 lines(x=flow$Date,y=flow$Q, col="steelblue")
 points(x=flow$Date,y=flow$Q, pch=19, col="steelblue", cex=0.4)
 axis(2, col.axis="blue", las=1)
 axis.Date(1,flow$Date,col.axis="blue",format="%b-%y",at=seq(as.Date(flow$Date[1]),as.Date(flow$Date[length(flow$Date)]),"months"))
 axis.Date(1,flow$Date,side=3,labels=F,at=seq(as.Date(flow$Date[1]),as.Date(flow$Date[length(flow$Date)]),"months"))
 title.txt<-paste("St.Joseph River:  total_pcb (",datetext,")")
 title(main=title.txt, font.main=2, col.main="red",line=3)
 mtext(expression("Load: 13.7 Kg" %+-% "12.0 Kg     RMSE: 2.8138 Kg     Annual Load: 13.7 Kg" %+-% "12.0 Kg     Annual Load (jackknife): 13.0 Kg" %+-% "11.8 Kg"),side=3,line=1,font=2,cex=0.9)
 par(new=TRUE)
 plot(x=flow$Date,y=dummy, type="n", axes=FALSE, ann=FALSE)
 axis(4, col.axis="blue", las=1)
 mtext(" TOTAL_PCB, IN NANOGRAMS PER LITER",side=4,line=4,font=2,cex=1.1)
 abline(v=conc$Date,col="grey80")
 points(x=conc$Date,y=conc$Conc, pch=22, bg="red", cex=1.2)
 if(mean(conc$Conc)<0.1) {
text(x=conc$Date+2,y=conc$Conc + 0.240, sprintf("%.3f",conc$Conc),cex=0.6,srt=90)
 } else {
text(x=conc$Date+2,y=conc$Conc + 0.240, sprintf("%.2f",conc$Conc),cex=0.6,srt=90)
 }
 abline(v=bound$Date+1.5,col="red",lty=3,lwd=2)
 text(x=strata$Date[1],y=0.97*max(dummy),"10.2 Kg",srt=90,cex=0.7,col="red")
 text(x=strata$Date[2],y=0.97*max(dummy),"2.6638 Kg",srt=90,cex=0.7,col="red")
 text(x=strata$Date[3],y=0.97*max(dummy),"0.88847 Kg",srt=90,cex=0.7,col="red")
 box()
 leg.txt<-c("DISCHARGE","CONCENTRATION","STRATA BOUNDARY")
 leg.col<-c("steelblue","black","red")
 leg.cex<-0.8
 leg.lty<-c(1,NA,3)
 leg.lwd<-c(1,NA,2)
 leg.pch<-c(19,22,NA)
 pt.bg<-c("black","red","black")
 par(mar=c(3,0,0,0))
 plot(1:10,1:10,type="n",axes=F,ann=F)
 mtext("Flow filename: St_Joseph_River_discharge_dv_2015-01-01_2015-12-31.txt      Concentration filename: St.Joseph_River__total_pcb__2015-01-01_2015-12-31.txt",side=1,line=1,font=1,cex=0.7)
 legend("center",legend=leg.txt,lty=leg.lty,col=leg.col,pch=leg.pch,lwd=leg.lwd,cex=leg.cex,pt.bg=pt.bg,inset=c(0.02,0.02),title="EXPLANATION")
 dev.off()
