USE [IMDB]
GO
/****** Object:  StoredProcedure [dbo].[GetAvailableSeats]    Script Date: 07-12-2016 03:05:21 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
create procedure [dbo].[GetAvailableSeats]
@MovieName char(1000), @Date Date, @TheatreName char(100)
as
begin
	select m.[Movie Name] as MovieName,
		th.TheatreName,
		st.ShowDate as Date,
		st.ShowTime as Time,
		sc.ScreenNumber as screenNumber,
		s.[Seat Number] as seatNumber
	from Movie m,
		Theatre th,
		ShowTiming st,
		Movie_Screen ms,
		Screen sc,
		seat s
	where m.[Movie Name] = @MovieName
		and st.ShowDate = @Date
		and th.TheatreName = @TheatreName
		and st.MovieID = m.MovieID
		and st.ScreenID = sc.ScreenID
		and sc.TheatreID = th.TheatreID
		and ms.MovieID = m.MovieID
		and ms.ScreenID = sc.ScreenID
		and s.ScreenID = sc.ScreenID
		and s.SeatID not in (select t.SeatID 
							from Movie m1,
								Theatre th1,
								ShowTiming st1,
								Movie_Screen ms1,
								Screen sc1,
								seat s1,
								Ticket t
							where m.[Movie Name] = @MovieName
								and st1.ShowDate = @Date
								and th1.TheatreName = @TheatreName
								and st1.MovieID = m1.MovieID
								and st1.ScreenID = sc1.ScreenID
								and sc1.TheatreID = th1.TheatreID
								and ms1.MovieID = m1.MovieID
								and ms1.ScreenID = sc1.ScreenID
								and s1.ScreenID = sc1.ScreenID
								and t.SeatID = s1.SeatID
  )
end;