-- ������� ������� ����������� �������� � �������� ������� (TRUE, FALSE)
create or replace function last_date (timestamp, timestamp) returns numeric as 
$$begin
if $1 = $2  
	then 
	return 1;
else 
	return 0;
end if; end;
$$ language plpgsql;

-- ������� ��� ������������� abc (�� ����� ���� � ������� � �����������)
create or replace function abc (numeric) returns varchar as 
$$begin
if $1 <= 50
	then 
	return 'A';
elsif $1 > 50 and $1 <=80
	then
	return 'B';
else 
	return 'C';
end if; end;
$$ language plpgsql;

-- ������� ��� ����������� �� ����� 
create or replace function split_by_date_2017 (timestamp, numeric) returns numeric as 
$$begin
if $1 >= '2017-01-01' and $1 <= '2017-12-31'
	then 
	return $2;
else 
	return 0;
end if; end;
$$ language plpgsql;

create or replace function split_by_date_2018 (timestamp, numeric) returns numeric as 
$$begin
if  $1 >=  '2018-01-01' and $1 <= '2018-12-31'
	then
	return $2;
else 
	return 0;
end if; end;
$$ language plpgsql;

create or replace function split_by_date_2019 (timestamp, numeric) returns numeric as 
$$begin
if  $1 >=  '2019-01-01' and $1 <= '2019-12-31'
	then
	return $2;
else 
	return 0;
end if; end;
$$ language plpgsql;

create or replace function split_by_date_2020 (timestamp, numeric) returns numeric as 
$$begin
if  $1 >= '2020-01-01' and $1 <= '2020-12-31'
	then
	return $2;
else 
	return 0;
end if; end;
$$ language plpgsql;
 
-- ������� ��� ������������� xyz (�� ����� ����������� �������� )
create or replace function xyz (numeric) returns varchar as 
$$begin
if $1 <= 25
	then 
	return 'X';
elsif $1 > 25 and $1 <= 50
	then
	return 'Y';
elseif $1 > 50 and $1 <= 100
	then 
	return 'Z';
else
	return 'Z1';
end if; end;
$$ language plpgsql;

-- ������� ��� ���������� ������� ������� 
create or replace function period_crysis("period" timestamp , date_sale timestamp) returns interval as 
$$begin
if $1 < '2020-04-01 00:00:00' and $2 >= '2020-07-01 00:00:00'
	then 
	return $2-$1-interval '90 DAYS';
else
	return $2-$1;
end if; end;
$$ language plpgsql;

-- ������� ��� ������������� abc �� ��������������� 
create or replace function abc_turnover(turnover_days double precision) returns varchar as 
$$begin
if $1 <= (select percentile_disc(0.33) within group (order by t2.turnover_days) from turnover_2 t2)
	then 
	return 'A_t' ;
elsif $1 > (select percentile_disc(0.33) within group (order by t2.turnover_days) from turnover_2 t2) and 
	$1 <= (select percentile_disc(0.66) within group (order by t2.turnover_days) from turnover_2 t2) 
	then 
	return 'B_t' ;
elsif $1 > (select percentile_disc(0.66) within group (order by t2.turnover_days) from turnover_2 t2) and 
	$1 <= (select percentile_disc(1.0) within group (order by t2.turnover_days) from turnover_2 t2)
	then 
	return 'C_t' ;
else
	return null;
end if; end;
$$ language plpgsql;


-- ������� ������� � ������������� � ������������� ������ 
with f as (select t.kolichestvo, t.sklad, t.summa,  
		t.period, t.guid_harakteristika,
	v.guid_nomenklatura, v.r3, v.razm_privedennyi, v.kategoriya, v.podkategoriya,	
	v.proba_prived, v.vid_izdel_prived, v.podvid_izdel, v.vstavka_prived, ------------------------------- �������� ����� 
	v.design_1, v.design_2, v.gr_vstavok_prived
			from tovaryvroznice t 
				left join ref_table v on v.guid = t.guid_harakteristika 
		where vid_dokumenta = '�����������������������'  
			or vid_dokumenta = '������������������' 
		),
	b as (select f.*, drh.link as size from f
		left join dopolnyt_rekvizit_haraktrazmer drh on drh.guid_harakteristika = f.guid_harakteristika
		where period > '2017-01-01 00:00:00')  -- ��� ������ ����� ����� � 01.01.2017 4769759  �����
select * 
	into for_abc_basic 
		from b
	where sklad not like '%c����%' 
and sklad not like '%�����%' and sklad not like '%������%' and sklad not like '%���%'
and sklad not like '%���%' and sklad not like '%����%' and sklad not like '%������%'
and sklad <> '����������� ����� ��'
; -- 2125449 �����



-- ������� ���������� ���� ��� ������� ����� ������� 
with t as (select max(rate_doll) as max_dol_price from rate_currency rc2),
v as (select *, round(fab.summa/rc.rate_doll*73.87, 2) as real_today_price 
		from for_abc_basic fab 
	left join rate_currency rc -- ������������ ������� � ������ ������� �� ������� � ��������� �������  
	on date_part('month', fab.period) = date_part('month', rc.date)
	and date_part('year', fab.period) = date_part('year', rc.date)),
s as (select *, count(kolichestvo) over (partition by 
	guid_nomenklatura,r3, razm_privedennyi, kategoriya, podkategoriya, vstavka_prived, gr_vstavok_prived,	 ------------------------------- �������� �����																							 
	proba_prived, vid_izdel_prived, podvid_izdel, design_1, design_2, ---------------------------- ��������� ����������� ����� ���������� ��� ������������ ������ ��� ������� 
	 guid_harakteristika) as double,
	min(period) over (partition by 
	guid_nomenklatura, r3, razm_privedennyi, kategoriya, podkategoriya,	 ------------------------------- �������� ����� 
	proba_prived, vid_izdel_prived, podvid_izdel,	design_1, design_2,  vstavka_prived,  gr_vstavok_prived,
	 guid_harakteristika) as min_date
	from v),
b as (select *, last_date(period,min_date) as first_action from s where last_date(period,min_date) = 1),
c as (select period, kolichestvo, sklad, summa, guid_nomenklatura, r3, razm_privedennyi, kategoriya, podkategoriya,	
proba_prived, vid_izdel_prived, podvid_izdel, design_1, design_2, vstavka_prived,  gr_vstavok_prived, ------------------------------- �������� ����� 
	 guid_harakteristika,
	real_today_price
		from b
	group by 
	period, kolichestvo, sklad, summa, guid_nomenklatura, r3, razm_privedennyi, kategoriya, podkategoriya,	
proba_prived, vid_izdel_prived, podvid_izdel,	design_1, design_2,   vstavka_prived,  gr_vstavok_prived,------------------------------- �������� ����� 
	guid_harakteristika, 
	real_today_price
	) 
select * into for_abc_basic_2 from c;

 

-- ������� ������� ������ � ����������� �� ����������� ���� ���� 
alter table for_abc_basic_2 add column price_prived numeric;

update for_abc_basic_2 set price_prived = rp.less_or_equal 
	from range_price rp
where rp.more_than < real_today_price and rp.less_or_equal >= real_today_price; 


-- ������� ������ ������� abc ������� � ������ ����� 
drop table abc_basic;

--������������ � ������� � ���������� � ������� �� ��������������� �������������� ����������, ����, � ���� �������. ����������� ������ A, B ��� C 
with v as (select t.period as date_sale, t.kolichestvo as count_sales, t.summa as price_sale, round(t.summa/rc.rate_doll*73.87, 2) as sales_today_price, 
		h.vstavka_prividennaya , h.gruppa_vstavok, t.guid_harakteristika  from tovaryvroznice t 
		left join harakteristika h on h.guid = t.guid_harakteristika
		left join rate_currency rc on date_part('month', t.period) = date_part('month', rc.date) -- �������� ����� � ��� �� ���� 
									and date_part('year', t.period) = date_part('year', rc.date)
		where vid_dokumenta='�����������������������'
	 	and period > '2017-01-01 00:00:00'
	 	),
z as (select fab.*, v.date_sale, v.count_sales, v.price_sale, v.sales_today_price 
		from for_abc_basic_2 fab
		left join v on fab.guid_harakteristika = v.guid_harakteristika 
		where -- fab.vid_izdel_prived  = '�������' -- or ------------------------------- �������� ����� (�������� ������ ������, ������� ����� �������������)
 			fab.vid_izdel_prived = '�����������' 
		),
 	l as (select *, sum(sales_today_price) over (partition by --- ���������� ������ �� ������� ����� ������������� ����������� 
 	r3, kategoriya, podvid_izdel, razm_privedennyi, --design_1, design_2,  ------------------------------- �������� ����� 
	 price_prived
 	) as turnover_by_grupp,
		sum(sales_today_price) over (partition by kolichestvo) as sum_turnover
		from z),
	x as (select *, turnover_by_grupp/sum_turnover*100 as share_on_sum_percent from l),
	m as (select distinct share_on_sum_percent from x order by share_on_sum_percent),
	xt as (select *, coalesce(sum(share_on_sum_percent) over (order by share_on_sum_percent desc), 0) as commutive_total from m) -- ������� ������������� ������ 
select x.*, xt.commutive_total, abc(xt.commutive_total) group_abc
	into abc_basic	
	from x 
	inner join xt on xt.share_on_sum_percent = x.share_on_sum_percent
	where sklad <> '����������� ����� ��'
	order by share_on_sum_percent desc; -- 83831


-- ������� ������ ������� xyz ������� � ������� �����, ������� ����������� �������� �� ����� �� ���� ������ ��������������� � ������ 
drop table xyz_basic;
	
with a as (select *, split_by_date_2017(date_sale, sales_today_price) as turnover_2017,
		split_by_date_2018(date_sale, sales_today_price) as turnover_2018,
		 split_by_date_2019(date_sale, sales_today_price) as turnover_2019, 
		 split_by_date_2020(date_sale, sales_today_price) as turnover_2020
		from abc_basic),		
	b as (select period, kolichestvo, count_sales, date_sale,	sklad,	summa,	guid_nomenklatura,	
		r3, kategoriya, podvid_izdel, razm_privedennyi, price_prived, --design_1, design_2,  ------------------------------- �������� ����� 
		--design_1,	design_2,	
		guid_harakteristika,	real_today_price,	
		sales_today_price,	turnover_by_grupp,	
		share_on_sum_percent,	commutive_total,	group_abc, 
		sum(turnover_2017) over (partition by r3, kategoriya, podvid_izdel, razm_privedennyi,
		--design_1, design_2,  ------------------------------- �������� ����� 
		price_prived
		) as turnover_2017,
		sum(turnover_2018) over (partition by r3, kategoriya, podvid_izdel, razm_privedennyi,  price_prived 	--design_1, design_2,  ------------------------------- �������� ����� 
		) as turnover_2018,
		sum(turnover_2019) over (partition by r3, kategoriya, podvid_izdel, razm_privedennyi, price_prived 	--design_1, design_2, ------------------------------- �������� ����� 
	) as turnover_2019,
		sum(turnover_2020) over (partition by r3, kategoriya, podvid_izdel, razm_privedennyi, price_prived	--design_1, design_2,  ------------------------------- �������� ����� 
	) as turnover_2020
				from a), 
	c as (select *, turnover_2017+turnover_2018+turnover_2019+turnover_2020 as turnover_in_4_years, 
		(turnover_2017+turnover_2018+turnover_2019+turnover_2020)/4 as avg_turnover_4_years,
		round((sqrt( power(turnover_2017 -((turnover_2017+turnover_2018+turnover_2019+turnover_2020)/4),2)+
		power(turnover_2018 -((turnover_2017+turnover_2018+turnover_2019+turnover_2020)/4),2)+
		power(turnover_2019 -((turnover_2017+turnover_2018+turnover_2019+turnover_2020)/4),2)+
		power(turnover_2020 -((turnover_2017+turnover_2018+turnover_2019+turnover_2020)/4),2)/4)/
		((turnover_2017+turnover_2018+turnover_2019+turnover_2020)/4))*100,1) as cv -- ������� ����������� �������� ��� xyz
		from b
		where turnover_2017+turnover_2018+turnover_2019+turnover_2020 > 0) -- ��������� ������ �� ���� ������� ������� �� 0
select *, xyz(cv) as xyz into xyz_basic from c; 



update xyz_basic set count_sales = coalesce(count_sales,0); -- ����������� �� NULL 



--- ������� ������ � ������� ������� ��� ������� ���������������
drop table turnover;

-- ���������������_2 
with t as (select guid_nomenklatura, guid,
			r3, kategoriya, podvid_izdel, razm_privedennyi,    ------------------------------- �������� ����� 
			date_prodazhi, date_ostatki, date_summary, kolichestvo, ostatki
from join_table_3 
where date_ostatki >= '2020-08-01 00:00:00'  -- ������� �� ��� 
	and  vid_izdel_prived = '�����������'  -- or ------------------------------- �������� ����� 
	-- podvid_izdel in ('������� ������', '����') -- in ('����', '���� �������')
	and t_t not like '%�������%' 
		and t_t not like '%���%'
		and t_t not like '%������%'
		and t_t not like '%������������%'
		and t_t not like '%������%'
		and t_t not like '%����%'
		and t_t not like '%����%'
		and t_t not like '%��������%'
		and t_t not like '%�����������%'
		and t_t not like '%�����������%'
		and t_t not like '%��������%'
		and t_t notnull 
		and gr_sklad not like '������'
		and t_t not like '%�����%'
		and t_t not like '%�����%'
		and t_t not like '%������%'), 
b as (select t.*, tb.summa, round(tb.summa/rc.rate_doll*73.87, 2) as sales_today_price 
	from t  		
left join (select summa, max("period"), guid_harakteristika 
			from tovaryvroznice
			where vid_dokumenta='�����������������������'
		 		and period > '2020-01-01 00:00:00'
			group by guid_harakteristika, summa 
			) tb on tb.guid_harakteristika = t.guid
left join rate_currency rc on date_part('month', t.date_summary) = date_part('month', rc.date) 
							and date_part('year', t.date_summary) = date_part('year', rc.date)	
			)
select * into turnover from b; 


	
-- ����������� ����������� ���� 
alter table turnover add column price_prived numeric;

update turnover set price_prived = rp.less_or_equal 
	from range_price rp
where rp.more_than < sales_today_price and rp.less_or_equal >= sales_today_price; 



-- ������� ������ �������� � �������� ------------------
	
drop table turnover_2;

with tb as (select   r3, kategoriya, podvid_izdel, razm_privedennyi, --	design_1, design_2, ------------------------------- �������� ����� (����� ���������� �� �������� �������)
			date_part('month', date_summary) as month, 
			date_part('year', date_summary) as year,    
		 	sum(kolichestvo) as kolichestvo, sum(ostatki) as ostatki 
		from turnover -- where date_summary >= '2020-07-01 00:00:00' -- and kolichestvo <> ostatki
	group by r3, kategoriya, podvid_izdel, razm_privedennyi,  price_prived,
															--design_1, design_2,  ------------------------------- �������� �����  
			 date_part('month', date_summary), date_part('year', date_summary)), 
tb_2 as (select r3, kategoriya, podvid_izdel, razm_privedennyi, price_prived,
		"month",	"year",  --design_1, design_2, ------------------------------- �������� ����� 
	case when 
	year = date_part('year',(select max(date_summary) from turnover)) and 
	month = date_part('month',(select max(date_summary) from turnover)) or 
	year = date_part('year',(select min(date_summary) from turnover)) and 
	month = date_part('month',(select min(date_summary) from turnover)) then 
	sum(ostatki)/2 else 
			sum(ostatki) end as sum_ostatki, sum(kolichestvo) sum_sales, sum(ostatki) as sum_ostatki_2  -- ����� ������ �� ������ � ����� ������� �������, ��������� ����� ���������
	from tb
group by r3, kategoriya, podvid_izdel, razm_privedennyi, price_prived,	
		"month",	"year"  --design_1, design_2 ------------------------------- �������� ����� 
	)
select sum(sum_ostatki)/11*12*30.5/nullif(sum(sum_sales),0)  as turnover_days,   -- ������ � ����������� �� ���������� �����������, ������� ��������������� ��� ������� ��������������� 
		sum(sum_sales)/nullif(sum(sum_ostatki)/11,0) as turnover_count_b_period, -- ���������� �������� ������ �� ��������������� ������  
		r3, kategoriya, podvid_izdel, razm_privedennyi,
		sum(sum_sales) as sales, sum(sum_ostatki) as ostatki --design_1, design_2 ------------------------------- �������� ����� 
	into turnover_2  
	from tb_2 
	where sum_sales <> 0  
		group by r3, kategoriya, podvid_izdel, razm_privedennyi, price_prived;
			 -- design_1, design_2;  ------------------------------- �������� ����� 




-- ���������������_1
-- ����������� ������������ ���������������,  ������� �������� ������, �������� �������� ������ 
with a as (select period, kolichestvo, count_sales, date_sale,	sklad,	summa,	guid_nomenklatura,	
		r3, kategoriya, podvid_izdel, razm_privedennyi, price_prived,	--design_1,	design_2,	 ------------------------------- �������� ����� 
		guid_harakteristika,	real_today_price,	sales_today_price,	turnover_by_grupp,	
		share_on_sum_percent,	commutive_total,	group_abc, cv, xyz from xyz_basic),
e as (select *, period_crysis("period", date_sale) as time_for_sale, count(kolichestvo) over 
		(partition by r3, kategoriya, podvid_izdel, razm_privedennyi, price_prived	-- design_1, design_2,  ------------------------------- �������� ����� 
			) as kolichestvo_v_gruppe 
		from a 
	where  date_sale < '2020-04-01 00:00:00' and period_crysis("period", date_sale) > interval '0 DAYS' and count_sales >=0
	or date_sale >= '2020-07-01 00:00:00' and period_crysis("period", date_sale) > interval '0 DAYS' and count_sales >=0
	order by time_for_sale asc ),	
r as (select  r3, kategoriya, podvid_izdel, razm_privedennyi,	
		kolichestvo_v_gruppe, --design_1, design_2,	                                 ------------------------------- �������� ����� 
		price_prived, group_abc, xyz, avg(time_for_sale) as  time_for_sale
			from e  
group by r3, kategoriya, podvid_izdel, razm_privedennyi, 
		kolichestvo_v_gruppe,  		                                    ------------------------------- �������� ����� 
		price_prived, group_abc, xyz ),
j as (	select r.*, 
		case when time_for_sale <= (select percentile_disc(0.33) within group (order by r.time_for_sale) from r) then 'A_tfs'  -- ���� �������� ������ 33 ����������: ����������� ����� A � ����� �� �������� 
					else ( case when time_for_sale > (select percentile_disc(0.33) within group (order by r.time_for_sale) from r) and 
										time_for_sale <= (select percentile_disc(0.66) within group (order by r.time_for_sale)from r ) then 'B_tfs' 
					else ( case when time_for_sale > (select percentile_disc(0.66) within group (order by r.time_for_sale) from r) and 
										time_for_sale <= (select percentile_disc(1.0) within group (order by r.time_for_sale)from r ) then 'C_tfs' 
					else null end ) end) end as abc_time_for_sale,
		round(t2.turnover_days) as oborachivaemost_days, 
		abc_turnover(round(t2.turnover_days)) as gruppa_po_oborachivaemosti
		from r
		left join turnover_2 t2 on  t2.kategoriya = r.kategoriya and  t2.razm_privedennyi = r.razm_privedennyi and 
				 t2.price_prived = r.price_prived and  t2.podvid_izdel = r.podvid_izdel and --t2.design_1 = r.design_1 and t2.design_2 = r.design_2 and ------------------------------- �������� ����� 
				 t2.r3 = r.r3 	 )
select * from j; 
				


