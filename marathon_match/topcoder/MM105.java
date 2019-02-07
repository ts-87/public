package mm;

import java.io.*;
import java.util.*;

public class MessageChecksum {
	private static final int comb[]={1,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,2,1,0,0,0,0,0,0,0,1,3,3,1,0,0,0,0,0,0,1,4,6,4,1,0,0,0,0,0,1,5,10,10,5,1,0,0,0,0,1,6,15,20,15,6,1,0,0,0,1,7,21,35,35,21,7,1,0,0,1,8,28,56,70,56,28,8,1,0,1,9,36,84,126,126,84,36,9,1};
	private static final double BORDER=1.0/1000000;
	private double pER=0.0;
	private double prop[],cp[],esub[];
	private int ccnt[],rcnt[][],ckcnt[];
	private int ans[];
	private String fans="";
	private int spnum,slim;
	private boolean used[],used2[];
	private int delay[];
    public String receiveMessage(int length, String s) {
		ccnt=new int[27];
		rcnt=new int[length][27];
		ckcnt=new int[length];
		prop=new double[27];
		cp=new double[27];
		esub=new double[27];
		HashMap<Double,Integer> errsub=new HashMap<>();
		delay=new int[length];
		Arrays.fill(delay,-1);
		for(int i=0;i<27;++i){
			prop[26-i]=(double)((i+1)*(i+1)-i*i)/729;
		}
		
		ccnt[26]=length-s.length();
		spnum=ccnt[26];
		for(int i=0;i<s.length();++i) ++ccnt[s.charAt(i)-'A'];
		errsub.put((double)ccnt[26],26);
		esub[26]=(double)ccnt[26];
		for(int i=0;i<26;++i){
			errsub.put(-(double)length/26+ccnt[i],i);
			esub[i]=-(double)length/26+ccnt[i];
		}
		Arrays.sort(esub);
		int pcnt=0;
		for(int i=0;i<27;++i){
			int id=errsub.get(esub[26-i]);
			
			if(id!=26){
				cp[id]=prop[i];
				
				if((i>=3&&i<=7)||(i>=19&&i<=23)){
					pER+=esub[26-i]/(prop[i]*length*25/26-(1.0-prop[i])*length/26);
					++pcnt;
				}
				
			}
		}
		pER/=pcnt;
		//System.err.println(pER);
		pER=Math.max(0.01,Math.min(0.5,pER));

		ans=new int[length+1];
		used=new boolean[length];
		used2=new boolean[length];
		slim=2;
		int ckrng=10;
		boolean dlck=false;
		
		if(pER>0.35){
			int ercnt=1000;
			if(spnum==0){
				for(int i=0;i<length;++i){
					++rcnt[i][s.charAt(i)-'A'];
					delay[i]=0;
				}
			}
			
			for(int i=0;i<length;++i){
				if(dlck&&i+ckrng<length){
					int tcksm=0;
					for(int j=0;j<ckrng;++j){
						tcksm+=(int)(s.charAt(j+i)-'A');
					}
					int corrcs=Sender.getChecksum(i,ckrng);
					if(tcksm%26==corrcs){
						for(int j=0;j<ckrng;++j){
							ans[i+j]=(int)(s.charAt(j+i)-'A');
						}
						i+=ckrng-1;
						continue;
					}
				}
				if(i==50){
					ercnt=1000;
					int[][] dp=new int[51][51];
					for(int j=0;j<50;++j){
						for(int k=0;k<50;++k){
							if(j==0) dp[j][k]=k;
							else if(k==0) dp[j][k]=j;
							else if(ans[j]==(int)(s.charAt(k)-'A')) dp[j][k]=dp[j-1][k-1];
							else dp[j][k]=Math.min(dp[j-1][k-1],Math.min(dp[j-1][k],dp[j][k-1]))+1;
						}
					}
					ercnt=dp[49][49];
					/*
					for(int j=0;j<50;++j){
						int tmp=0;
						for(int k=0;k<50;++k){
							if(j==k) continue;
							int pad=0;
							if(k>j) pad=1;
							if(ans[k]!=(int)(s.charAt(k-pad)-'A')) ++tmp;
						}
						if(tmp<ercnt) ercnt=tmp;
					}
					*/
					if(ercnt<=9){
						slim=1;
					}
					if(spnum>0&&(double)ercnt/50<0.25){
						
						int interval=length/(spnum*4);
						int presp=0;
						int pos=50+interval;
						int wd=6;
						while(interval+pos<length){
							for(int ii=pos;ii<pos+wd;++ii){
								if(!used[ii])check(ii);
								used[ii]=true;
							}
							int mx=0,prsp=0,nxsp=0;
							for(int ii=wd;ii>=wd;--ii){
								int same=0;
								for(int j=0;j<=wd;++j){
									if(ii==j) continue;
									int pad=0;
									if(j>ii) pad=1;
									if(ans[j+pos]==(int)(s.charAt(j+pos-pad-presp)-'A')) ++same;
								}
								if(mx<same){
									mx=same;
									if(ii!=wd){
										nxsp=1;
									}
									if(ii==0) {
										++nxsp;
										prsp=1;
									}
								}	
							}
					
							for(int ii=wd;ii>=wd;--ii){
								int same=0;
								for(int j=0;j<=wd-1;++j){
									if(ii==j) continue;
									int pad=1;
									if(j>ii) pad=2;
									if(ans[j+pos]==(int)(s.charAt(j+pos-pad-presp)-'A')) ++same;
								}
								if(mx<same){
									mx=same;
									nxsp=1;
									prsp=1;
									if(ii!=wd) nxsp=2;
									if(ii==0){
										++nxsp;
										prsp=2;
									}
								}
							}
							for(int ii=wd;ii>=wd;--ii){
								int same=0;
								for(int j=0;j<=wd-2;++j){
									if(ii==j) continue;
									int pad=2;
									if(j>ii) pad=3;
									if(ans[j+pos]==(int)(s.charAt(j+pos-pad-presp)-'A')) ++same;
								}
								if(mx<same){
									mx=same;
									nxsp=2;
									prsp=2;
									if(ii!=wd) nxsp=2;
									if(ii==0){
										++nxsp;
										prsp=2;
									}
								}
							}
							for(int ii=wd;ii>=wd;--ii){
								int same=0;
								for(int j=0;j<=wd-3;++j){
									if(ii==j) continue;
									int pad=3;
									if(j>ii) pad=4;
									if(ans[j+pos]==(int)(s.charAt(j+pos-pad-presp)-'A')) ++same;
								}
								if(mx<same){
									mx=same;
									nxsp=3;
									prsp=3;
									if(ii!=wd) nxsp=2;
									if(ii==0){
										++nxsp;
										prsp=2;
									}
								}
							}
					
							if(mx>=3){
								if(prsp==0){
									for(int j=pos-interval;j<pos+wd;++j){
										++rcnt[j][s.charAt(j-presp)-'A'];
										delay[j]=presp;
									}
								}
								else{
							
									//for(int j=pos-interval;j<pos;++j){
									//	int sub=(j-(pos-interval))/((prsp+1)*interval);
									//	++rcnt[j][s.charAt(j-presp-sub)-'A'];
									//	delay[j]=presp;
									//}
							
								}
								presp+=nxsp;
								presp=Math.min(presp,spnum);
							}
							else{
								presp+=nxsp;
								presp=Math.min(presp,spnum);
							}
					
							if(presp==spnum){
								for(int j=pos+wd;j<length;++j){
									++rcnt[j][s.charAt(j-presp)-'A'];
								}
								break;
							}
					
							pos+=interval;
						}
						
					}
					
					if(ercnt<=1&&spnum==0){
						dlck=true;
						for(int j=50;j<length;++j){
							ans[j]=(int)(s.charAt(j)-'A');
						}
						break;
					}
				}
				/*
				if(i==100){
					int[][] dp=new int[101][101];
					for(int j=0;j<100;++j){
						for(int k=0;k<100;++k){
							if(j==0) dp[j][k]=k;
							else if(k==0) dp[j][k]=j;
							else if(ans[j]==(int)(s.charAt(k)-'A')) dp[j][k]=dp[j-1][k-1];
							else dp[j][k]=Math.min(dp[j-1][k-1],Math.min(dp[j-1][k],dp[j][k-1]))+1;
						}
					}
					if(dp[99][99]<=1){
						slim=2;
						break;
					}
				}
				*/
				if(i>=50&&(double)ercnt/50<0.25){
					if(!used[i])check(i);
				}
				else{
					int cksm=Sender.getChecksum(i,1);
					ans[i]=cksm;
				}
				/*
				if(i<50){
					if(i<s.length()){
						if(ans[i]!=(int)(s.charAt(i)-'A')) ++ercnt;
					}
					else ++ercnt;
				}
				*/
			}
			for(int loop=0;loop<4;++loop){
				int totck=0;
				for(int i=0;i<length;++i){
					totck+=ans[i];
				}
				if((double)ercnt/50<0.25){
					int cksm=Sender.getChecksum(0,length);
					
					if(cksm!=totck%26){
						if(loop==0){
							++slim;
						}
						if(loop>=3){
							for(int i=0;i<length;++i){
								if(i>=50)check(i);
							}
						}
						else{
							int sqlen=(int)Math.sqrt((double)length);
							int srng=sqlen/2,crng=length/srng;//sqlen*2;
							for(int i=0;i<crng;++i){
								if(!used2[i]){
									int r=srng*i+srng;
									if(i==crng-1) r=length;
									cksm=Sender.getChecksum(srng*i,r-srng*i);
									totck=0;
									for(int j=srng*i;j<r;++j){
										totck+=ans[j];
									}
									if(totck%26!=cksm){
										if(srng/2<10){
											for(int j=srng*i;j<r;++j){
												if(j>=50)check(j);
											}
										}
										else{
											int l1=srng*i,r1=l1+srng/2,r2=r;
											cksm=Sender.getChecksum(l1,r1-l1);
											totck=0;
											for(int j=l1;j<r1;++j){
												totck+=ans[j];
											}
											if(totck%26!=cksm){
												for(int j=l1;j<r1;++j){
													if(j>=50)check(j);
												}
											}
											cksm=Sender.getChecksum(r1,r2-r1);
											totck=0;
											for(int j=r1;j<r2;++j){
												totck+=ans[j];
											}
											if(totck%26!=cksm){
												for(int j=r1;j<r2;++j){
													if(j>=50)check(j);
												}
											}
										}
									}
									else used2[i]=true;
								}
							}
							/*
							for(int i=0;i<sqlen;++i){
								if(!used2[i]){
									int r=sqlen*i+sqlen;
									if(i==sqlen-1) r=length;
									cksm=Sender.getChecksum(sqlen*i,r-sqlen*i);
									totck=0;
									for(int j=sqlen*i;j<r;++j){
										totck+=ans[j];
									}
									if(totck%26!=cksm){
										for(int j=sqlen*i;j<r;++j){
											if(j>=50)check(j);
										}
									}
									else used2[i]=true;
								}
							}
							*/
						
						}
					}
					else break;
				}
				else break;
			}
		}
		
		else{
			if(spnum==0){
				for(int i=0;i<length;++i){
					++rcnt[i][s.charAt(i)-'A'];
					delay[i]=0;
				}
			}
			
			else{
				int interval=length/(spnum*4);
				int presp=0;
				int pos=interval;
				int wd=6;
				while(interval+pos<length){
					for(int i=pos;i<pos+wd;++i){
						if(!used[i])check(i);
						used[i]=true;
					}
					int mx=0,prsp=0,nxsp=0;
					for(int i=wd;i>=wd;--i){
						int same=0;
						for(int j=0;j<=wd;++j){
							if(i==j) continue;
							int pad=0;
							if(j>i) pad=1;
							if(ans[j+pos]==(int)(s.charAt(j+pos-pad-presp)-'A')) ++same;
						}
						if(mx<same){
							mx=same;
							if(i!=wd){
								nxsp=1;
							}
							if(i==0) {
								++nxsp;
								prsp=1;
							}
						}
					}
					
					for(int i=wd;i>=wd;--i){
						int same=0;
						for(int j=0;j<=wd-1;++j){
							if(i==j) continue;
							int pad=1;
							if(j>i) pad=2;
							if(ans[j+pos]==(int)(s.charAt(j+pos-pad-presp)-'A')) ++same;
						}
						if(mx<same){
							mx=same;
							nxsp=1;
							prsp=1;
							if(i!=wd) nxsp=2;
							if(i==0){
								++nxsp;
								prsp=2;
							}
						}
					}
					for(int i=wd;i>=wd;--i){
						int same=0;
						for(int j=0;j<=wd-2;++j){
							if(i==j) continue;
							int pad=2;
							if(j>i) pad=3;
							if(ans[j+pos]==(int)(s.charAt(j+pos-pad-presp)-'A')) ++same;
						}
						if(mx<same){
							mx=same;
							nxsp=2;
							prsp=2;
							if(i!=wd) nxsp=2;
							if(i==0){
								++nxsp;
								prsp=2;
							}
						}
					}
					for(int i=wd;i>=wd;--i){
						int same=0;
						for(int j=0;j<=wd-3;++j){
							if(i==j) continue;
							int pad=3;
							if(j>i) pad=4;
							if(ans[j+pos]==(int)(s.charAt(j+pos-pad-presp)-'A')) ++same;
						}
						if(mx<same){
							mx=same;
							nxsp=3;
							prsp=3;
							if(i!=wd) nxsp=2;
							if(i==0){
								++nxsp;
								prsp=2;
							}
						}
					}
					
					if(mx>=3){
						if(prsp==0){
							for(int j=pos-interval;j<pos+wd;++j){
								++rcnt[j][s.charAt(j-presp)-'A'];
								delay[j]=presp;
							}
						}
						else{
							
							//for(int j=pos-interval;j<pos;++j){
							//	int sub=(j-(pos-interval))/((prsp+1)*interval);
							//	++rcnt[j][s.charAt(j-presp-sub)-'A'];
							//	delay[j]=presp;
							//}
							
						}
						presp+=nxsp;
						presp=Math.min(presp,spnum);
					}
					else{
						presp+=nxsp;
						presp=Math.min(presp,spnum);
					}
					
					if(presp==spnum){
						for(int j=pos+wd;j<length;++j){
							++rcnt[j][s.charAt(j-presp)-'A'];
						}
						break;
					}
					
					pos+=interval;
				}
				
			}
			
			int ercnt=1000;
			for(int i=0;i<length;++i){
				if(dlck&&i+ckrng<length){
					int tcksm=0;
					boolean ok=true;
					for(int j=0;j<ckrng;++j){
						if(used[j+i]) tcksm+=ans[i+j]; 
						else if(delay[j+i]==-1) ok=false;
						else tcksm+=(int)(s.charAt(j+i-delay[i+j])-'A');
					}
					if(ok){
						int corrcs=Sender.getChecksum(i,ckrng);
						if(tcksm%26==corrcs){
							for(int j=0;j<ckrng;++j){
								if(!used[i+j]){
									ans[i+j]=(int)(s.charAt(j+i-delay[i+j])-'A');
									used[i+j]=true;
								}
							}
							i+=ckrng-1;
							continue;
						}
					}
				}
				if(!used[i])check(i);
				if(i==50){
					ercnt=1000;
					int[][] dp=new int[52][52];
					for(int j=0;j<51;++j){
						for(int k=0;k<51;++k){
							if(j==0) dp[j][k]=k;
							else if(k==0) dp[j][k]=j;
							else if(ans[j]==(int)(s.charAt(k)-'A')) dp[j][k]=dp[j-1][k-1];
							else dp[j][k]=Math.min(dp[j-1][k-1],Math.min(dp[j-1][k],dp[j][k-1]))+1;
						}
					}
					ercnt=dp[50][50];
					/*
					for(int j=0;j<51;++j){
						int tmp=0;
						for(int k=0;k<=50;++k){
							if(j==k) continue;
							int pad=0;
							if(k>j) pad=1;
							if(ans[k]!=(int)(s.charAt(k-pad)-'A')) ++tmp;
						}
						if(tmp<ercnt) ercnt=tmp;
					}
					*/
					if(ercnt<=9){
						slim=1;
						//return s;
					}
					if(ercnt<=1&&spnum==0){
						dlck=true;
						for(int j=50;j<length;++j){
							if(!used[j])ans[j]=(int)(s.charAt(j)-'A');
						}
						break;
					}
				}
				/*
				if(i==100){
					int[][] dp=new int[102][102];
					for(int j=0;j<101;++j){
						for(int k=0;k<101;++k){
							if(j==0) dp[j][k]=k;
							else if(k==0) dp[j][k]=j;
							else if(ans[j]==(int)(s.charAt(k)-'A')) dp[j][k]=dp[j-1][k-1];
							else dp[j][k]=Math.min(dp[j-1][k-1],Math.min(dp[j-1][k],dp[j][k-1]))+1;
						}
					}
					if(dp[100][100]<=1){
						slim=2;
						break;
					}
				}
				*/
				/*
				while(true){
					msg=Sender.getMessage(i,1);
					
					++ckcnt[i];
					if(msg.length()==0) continue;
					int id=msg.charAt(0)-'A';
					++rcnt[i][id];
					int scr=rcnt[i][id];
					double pr=Math.pow(pER*cp[id],scr);
					if(scr>slim){
						ans[i]=id;
						break;
					}
				}
				*/
			}
			for(int loop=0;loop<4;++loop){
				int totck=0;
				for(int i=0;i<length;++i){
					totck+=ans[i];
				}
				int cksm=Sender.getChecksum(0,length);
				if(cksm!=totck%26){
					if(loop==0){
						++slim;
					}
					if(loop>=3){
						for(int i=0;i<length;++i){
							check(i);
						}
					}
					else{
						int sqlen=(int)Math.sqrt((double)length);
						int srng=sqlen/2,crng=length/srng;//sqlen*2;
						for(int i=0;i<crng;++i){
							if(!used2[i]){
								int r=srng*i+srng;
								if(i==crng-1) r=length;
								cksm=Sender.getChecksum(srng*i,r-srng*i);
								totck=0;
								for(int j=srng*i;j<r;++j){
									totck+=ans[j];
								}
								if(totck%26!=cksm){
									if(srng/2<10){
										for(int j=srng*i;j<r;++j){
											check(j);
										}
									}
									else{
										int l1=srng*i,r1=l1+srng/2,r2=r;
										cksm=Sender.getChecksum(l1,r1-l1);
										totck=0;
										for(int j=l1;j<r1;++j){
											totck+=ans[j];
										}
										if(totck%26!=cksm){
											for(int j=l1;j<r1;++j){
												check(j);
											}
										}
										cksm=Sender.getChecksum(r1,r2-r1);
										totck=0;
										for(int j=r1;j<r2;++j){
											totck+=ans[j];
										}
										if(totck%26!=cksm){
											for(int j=r1;j<r2;++j){
												check(j);
											}
										}
									}
								}
								else used2[i]=true;
							}
						}
						/*
						for(int i=0;i<sqlen;++i){
							if(!used2[i]){
								int r=sqlen*i+sqlen;
								if(i==sqlen-1) r=length;
								cksm=Sender.getChecksum(sqlen*i,r-sqlen*i);
								totck=0;
								for(int j=sqlen*i;j<r;++j){
									totck+=ans[j];
								}
								if(totck%26!=cksm){
									for(int j=sqlen*i;j<r;++j){
										check(j);
									}
								}
								else used2[i]=true;
							}
						}
						*/
					}
				}
				else break;
			}
		}

		for(int i=0;i<length;++i) fans+=(char)('A'+ans[i]);
    	return fans;
		
	}
	private void check(int cur){
		
		String msg;
		while(true){
			msg=Sender.getMessage(cur,1);
			
			++ckcnt[cur];
			if(msg.length()==0) continue;
			int id=msg.charAt(0)-'A';
			++rcnt[cur][id];
			int scr=rcnt[cur][id];
			double pr=Math.pow(pER*cp[id],scr);
			if(scr>slim){
				ans[cur]=id;
				break;
			}
		}
	}
	
	
  	public static void main(String[] args) {
    	try {
      		MessageChecksum sol = new MessageChecksum();      
      		int length = Integer.parseInt(BR.readLine());
      		String message = BR.readLine();
      		System.out.println(sol.receiveMessage(length, message));
    	} catch (Exception e) {}
	}
	
	
	
}


class BR {
  	public static BufferedReader br;
  	static {
  		br = new BufferedReader(new InputStreamReader(System.in));
  	}
  	public static String readLine() {
    	try { return br.readLine(); } catch (IOException e) { return ""; }
  	}
}

class Sender {
  	public static int getChecksum(int start, int length) {
    	System.out.println("?getChecksum");
    	System.out.println(start);
    	System.out.println(length);
    	System.out.flush();
    	int ret = 0;
    	try {
      	ret = Integer.parseInt(BR.readLine());
    	} catch (NumberFormatException e) {}
    	return ret;
  	}

  	public static String getMessage(int start, int length) {
    	System.out.println("?getMessage");
    	System.out.println(start);
    	System.out.println(length);
    	System.out.flush();
    	return BR.readLine();
  	}
}
