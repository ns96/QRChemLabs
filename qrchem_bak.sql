-- --------------------------------------------------------
-- Host:                         192.168.1.45
-- Server version:               10.6.18-MariaDB - MariaDB package
-- Server OS:                    Linux
-- HeidiSQL Version:             12.9.0.6999
-- --------------------------------------------------------

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET NAMES utf8 */;
/*!50503 SET NAMES utf8mb4 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;


-- Dumping database structure for qrchem
DROP DATABASE IF EXISTS `qrchem`;
CREATE DATABASE IF NOT EXISTS `qrchem` /*!40100 DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci */;
USE `qrchem`;

-- Dumping structure for table qrchem.Courses
DROP TABLE IF EXISTS `Courses`;
CREATE TABLE IF NOT EXISTS `Courses` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `Code` varchar(5) NOT NULL DEFAULT '0',
  `Semester` varchar(25) NOT NULL DEFAULT '0',
  `Locker` varchar(25) NOT NULL DEFAULT '0',
  PRIMARY KEY (`ID`)
) ENGINE=InnoDB AUTO_INCREMENT=28 DEFAULT CHARSET=utf8mb3 COLLATE=utf8mb3_general_ci;

-- Dumping data for table qrchem.Courses: ~4 rows (approximately)
INSERT INTO `Courses` (`ID`, `Code`, `Semester`, `Locker`) VALUES
	(23, 'D021', 'SPRING_2025', 'A'),
	(25, 'E011', 'SPRING_2025', 'A'),
	(26, 'D01L', 'SUMMER_2025', 'A'),
	(27, 'D22L', 'FALL_2025', 'A');

-- Dumping structure for table qrchem.ExpData
DROP TABLE IF EXISTS `ExpData`;
CREATE TABLE IF NOT EXISTS `ExpData` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `Semester` varchar(50) DEFAULT NULL,
  `Pin` varchar(50) DEFAULT NULL,
  `ExpName` varchar(50) DEFAULT NULL,
  `Questions` text DEFAULT NULL,
  `SaveTime` timestamp NULL DEFAULT current_timestamp(),
  PRIMARY KEY (`ID`)
) ENGINE=InnoDB AUTO_INCREMENT=1185 DEFAULT CHARSET=utf8mb3 COLLATE=utf8mb3_general_ci;

-- Dumping data for table qrchem.ExpData: ~2 rows (approximately)
INSERT INTO `ExpData` (`ID`, `Semester`, `Pin`, `ExpName`, `Questions`, `SaveTime`) VALUES
	(1183, 'SPRING_2025', 'D0210123', 'EXP04D', '{"d.X":["0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10"],"d.Y":["88.0117,85.4922,83.5103,81.4991,79.8863,80.0583,80.1709,79.9291,80.0024,79.8913,79.8797,80.2988,78.9947,78.0404,77.4786,76.2373,73.8765,71.6995,69.4273,67.6196,65.4185"],"d.Y2":["Liquid,Liquid,Liquid,Liquid,Liquid,Liquid,Liquid+Solid,Liquid+Solid,Liquid+Solid,Liquid+Solid,Solid,Solid,Solid,Solid,Solid,Solid,Solid,Solid,Solid,Solid,Solid"],"d.FP":[79.9013],"q1":[80],"q2":["Vanillin (80-81)"]}', '2025-04-02 20:11:56'),
	(1184, 'SPRING_2025', 'D0210123', 'EXP04D', '{"d.X":["0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10"],"d.Y":["58.3445,55.8331,53.0788,51.2737,50.1292,50.1624,50.0554,50.1322,49.7388,49.992,49.9975,49.9745,49.2883,48.4517,47.728,45.7419,44.2689,41.8532,39.6311,37.8666,35.1593"],"d.Y2":["Liquid,Liquid,Liquid,Liquid,Liquid,Liquid,Liquid+Solid,Liquid+Solid,Liquid+Solid,Liquid+Solid,Solid,Solid,Solid,Solid,Solid,Solid,Solid,Solid,Solid,Solid,Solid"],"d.FP":[49.9411],"q1":[50],"q2":["Benzophenone (49-51)"]}', '2025-04-03 12:33:20');

/*!40103 SET TIME_ZONE=IFNULL(@OLD_TIME_ZONE, 'system') */;
/*!40101 SET SQL_MODE=IFNULL(@OLD_SQL_MODE, '') */;
/*!40014 SET FOREIGN_KEY_CHECKS=IFNULL(@OLD_FOREIGN_KEY_CHECKS, 1) */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40111 SET SQL_NOTES=IFNULL(@OLD_SQL_NOTES, 1) */;
