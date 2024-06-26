module LargeUnSat(largeUnSat) where

import MiniSat.Solver
import MiniSat.Parser

largeUnSat :: Model Int
largeUnSat = dimacsToModel file

file :: [String]
file = [
        "c This Formular is generated by mcnf",
        "c",
        "c    horn? no ",
        "c    forced? no ",
        "c    mixed sat? no ",
        "c    clause length = 3 ",
        "c",
        "p cnf 100  430 ",
        "-46 90 77 0",
        "-100 -61 -2 0",
        "5 -95 79 0",
        "-34 -56 -23 0",
        "80 66 70 0",
        "-82 54 -34 0",
        "-77 28 33 0",
        "36 -17 -7 0",
        "-40 -90 -25 0",
        "55 -59 -46 0",
        "68 56 8 0",
        "74 -91 -62 0",
        "18 5 91 0",
        "94 58 37 0",
        "-98 74 -43 0",
        "66 4 67 0",
        "-9 -100 -46 0",
        "-82 64 -89 0",
        "-57 -48 -100 0",
        "51 -37 40 0",
        "-14 89 -40 0",
        "-20 -15 77 0",
        "30 -74 14 0",
        "-100 30 71 0",
        "97 57 -70 0",
        "73 -18 43 0",
        "-89 -18 -65 0",
        "15 13 30 0",
        "52 31 -22 0",
        "-26 -93 -25 0",
        "35 14 -22 0",
        "35 -29 100 0",
        "-24 21 -42 0",
        "85 -53 -15 0",
        "40 -81 -93 0",
        "-37 92 -93 0",
        "18 31 -98 0",
        "-32 66 9 0",
        "-66 4 -83 0",
        "49 -57 52 0",
        "13 -95 21 0",
        "-68 95 -55 0",
        "-54 44 -62 0",
        "-93 -82 85 0",
        "-46 -10 58 0",
        "27 -64 -12 0",
        "71 37 -85 0",
        "-21 56 -40 0",
        "63 -10 -37 0",
        "48 30 -8 0",
        "23 -55 34 0",
        "-72 28 7 0",
        "-17 84 -42 0",
        "61 -11 34 0",
        "-55 -26 -38 0",
        "83 7 19 0",
        "39 -80 -38 0",
        "-49 -92 -50 0",
        "88 -50 -62 0",
        "-51 -12 83 0",
        "85 -54 42 0",
        "-14 -9 29 0",
        "-17 -3 -96 0",
        "-18 81 -36 0",
        "37 53 -3 0",
        "-54 26 -28 0",
        "-44 -43 -6 0",
        "-76 -11 -63 0",
        "-57 -95 38 0",
        "-84 11 91 0",
        "-61 -83 -58 0",
        "-86 -20 29 0",
        "51 68 33 0",
        "71 -96 -4 0",
        "47 -71 89 0",
        "70 18 -100 0",
        "13 -92 -96 0",
        "-64 33 -55 0",
        "5 18 -89 0",
        "-87 72 47 0",
        "-93 78 22 0",
        "-68 -17 19 0",
        "51 22 -34 0",
        "98 22 -87 0",
        "-18 95 9 0",
        "-85 -82 -34 0",
        "44 -2 -81 0",
        "-58 -81 -16 0",
        "60 20 94 0",
        "8 -67 11 0",
        "-28 43 -95 0",
        "24 -63 82 0",
        "86 -27 22 0",
        "-98 -49 -38 0",
        "8 2 54 0",
        "33 45 -55 0",
        "-8 30 36 0",
        "31 20 -52 0",
        "19 -68 -7 0",
        "18 -64 -48 0",
        "-69 26 -14 0",
        "-4 -62 23 0",
        "-23 16 -58 0",
        "-44 55 51 0",
        "81 69 91 0",
        "-85 75 -64 0",
        "-85 -68 -73 0",
        "-1 80 57 0",
        "-18 70 -77 0",
        "12 15 -51 0",
        "53 -16 -74 0",
        "67 -81 48 0",
        "-39 58 91 0",
        "28 50 76 0",
        "86 30 -24 0",
        "36 -84 -70 0",
        "90 67 46 0",
        "49 45 -62 0",
        "46 -68 -44 0",
        "-31 -58 90 0",
        "-9 30 5 0",
        "-43 -28 -72 0",
        "-92 -14 -22 0",
        "-47 99 -29 0",
        "51 57 -40 0",
        "-13 79 -67 0",
        "33 46 42 0",
        "-12 -25 40 0",
        "12 -76 29 0",
        "-51 46 85 0",
        "52 -63 14 0",
        "89 85 -77 0",
        "-49 -17 -12 0",
        "90 -93 -64 0",
        "47 44 -5 0",
        "-66 86 -17 0",
        "-10 92 -53 0",
        "-73 -70 -74 0",
        "73 -48 47 0",
        "-96 13 83 0",
        "-54 -9 2 0",
        "-39 -97 96 0",
        "48 55 87 0",
        "76 12 -93 0",
        "61 44 71 0",
        "-60 -97 23 0",
        "-65 -6 -60 0",
        "-44 -42 63 0",
        "-40 92 -99 0",
        "-23 84 28 0",
        "-25 -24 -64 0",
        "50 -96 -79 0",
        "86 36 -8 0",
        "-55 41 80 0",
        "10 -25 2 0",
        "79 -15 52 0",
        "-16 -49 37 0",
        "-60 -44 -65 0",
        "55 -46 6 0",
        "-53 8 33 0",
        "83 -25 37 0",
        "76 -83 84 0",
        "41 38 -85 0",
        "-71 -59 20 0",
        "85 46 -26 0",
        "-39 84 -96 0",
        "-11 93 46 0",
        "36 -46 -60 0",
        "95 -22 23 0",
        "-33 -83 -60 0",
        "30 39 -44 0",
        "87 25 -53 0",
        "-54 -3 -14 0",
        "-67 -75 99 0",
        "-13 -56 73 0",
        "100 94 77 0",
        "11 32 89 0",
        "97 -3 49 0",
        "-87 42 77 0",
        "-98 35 -5 0",
        "-4 6 -53 0",
        "16 -41 92 0",
        "-51 -80 44 0",
        "-1 -76 -60 0",
        "27 58 17 0",
        "90 32 -28 0",
        "-3 27 -98 0",
        "-35 -83 60 0",
        "-96 -83 -25 0",
        "-37 86 -41 0",
        "76 7 100 0",
        "-60 52 -38 0",
        "-37 -81 51 0",
        "-46 54 -15 0",
        "-54 -46 -61 0",
        "-63 -75 83 0",
        "74 -37 83 0",
        "79 30 -8 0",
        "-62 72 20 0",
        "51 -95 -75 0",
        "-88 52 49 0",
        "-2 -88 -62 0",
        "12 77 85 0",
        "86 68 83 0",
        "35 95 19 0",
        "84 -94 -59 0",
        "57 66 -60 0",
        "-73 -61 34 0",
        "68 -66 -8 0",
        "-74 5 -47 0",
        "-99 98 47 0",
        "-30 86 76 0",
        "-47 77 -86 0",
        "21 -1 32 0",
        "79 25 8 0",
        "77 -14 19 0",
        "34 -100 33 0",
        "35 13 -20 0",
        "-36 -47 18 0",
        "-42 21 27 0",
        "-43 51 66 0",
        "99 21 -45 0",
        "-42 -44 92 0",
        "-48 -95 -84 0",
        "85 -99 1 0",
        "59 -8 88 0",
        "-36 -72 29 0",
        "-60 -22 82 0",
        "-90 -31 -36 0",
        "-53 -80 -93 0",
        "-91 -49 64 0",
        "-87 -9 -16 0",
        "49 -10 -35 0",
        "-90 24 12 0",
        "-15 -99 -98 0",
        "59 38 -18 0",
        "23 19 -64 0",
        "92 -55 -61 0",
        "3 45 -77 0",
        "20 -21 -95 0",
        "-71 -47 22 0",
        "44 7 -50 0",
        "58 -37 -11 0",
        "72 -66 10 0",
        "44 99 37 0",
        "21 -86 -72 0",
        "73 17 61 0",
        "-78 -81 42 0",
        "54 70 -64 0",
        "-61 -48 17 0",
        "-19 48 -13 0",
        "-33 -39 29 0",
        "33 43 -76 0",
        "76 11 -49 0",
        "19 -88 45 0",
        "-76 -5 -55 0",
        "-57 -98 -15 0",
        "19 -86 -94 0",
        "82 -70 22 0",
        "-39 -85 -63 0",
        "50 75 25 0",
        "-96 -37 -15 0",
        "-99 -75 -10 0",
        "-67 -63 38 0",
        "-7 -22 -18 0",
        "-39 -45 62 0",
        "-64 -79 37 0",
        "-69 -58 -100 0",
        "-46 17 43 0",
        "17 -11 46 0",
        "-22 -19 -86 0",
        "84 45 48 0",
        "46 -91 64 0",
        "-25 -15 30 0",
        "92 14 45 0",
        "-79 -69 43 0",
        "-34 -35 -69 0",
        "87 1 -82 0",
        "-64 70 -65 0",
        "-27 -29 75 0",
        "95 45 90 0",
        "62 43 76 0",
        "21 -11 -31 0",
        "53 24 -46 0",
        "-78 43 -12 0",
        "-74 9 22 0",
        "-5 7 -73 0",
        "12 -57 -7 0",
        "-19 12 90 0",
        "-47 -6 -50 0",
        "-35 -32 72 0",
        "-87 -100 -24 0",
        "-56 92 -16 0",
        "29 53 87 0",
        "-25 64 -20 0",
        "37 57 9 0",
        "25 -100 -4 0",
        "7 -16 77 0",
        "-98 7 -64 0",
        "98 50 -44 0",
        "95 -60 23 0",
        "7 92 -35 0",
        "-95 -47 -9 0",
        "82 -67 93 0",
        "-25 -49 -66 0",
        "-34 -60 49 0",
        "91 -70 8 0",
        "20 75 -94 0",
        "15 -27 -30 0",
        "-24 -95 -29 0",
        "78 -98 53 0",
        "-43 -44 -86 0",
        "-98 89 -40 0",
        "-55 31 -12 0",
        "-64 60 77 0",
        "86 -27 -37 0",
        "55 96 34 0",
        "5 44 92 0",
        "30 -90 -39 0",
        "-23 38 -36 0",
        "-40 -16 -12 0",
        "-67 8 61 0",
        "-4 -52 -51 0",
        "4 -26 76 0",
        "-18 -89 87 0",
        "-39 27 -32 0",
        "-15 -69 94 0",
        "83 -72 -93 0",
        "-19 70 -14 0",
        "45 -30 -69 0",
        "-48 -22 -83 0",
        "-66 -87 55 0",
        "-100 -45 -96 0",
        "-81 -97 32 0",
        "75 -36 -73 0",
        "-98 -14 78 0",
        "66 59 -36 0",
        "44 17 43 0",
        "1 -89 16 0",
        "75 -58 42 0",
        "-4 -98 22 0",
        "-31 -44 -73 0",
        "64 7 6 0",
        "26 -45 -3 0",
        "-48 -52 44 0",
        "98 -3 40 0",
        "35 -15 61 0",
        "97 44 -30 0",
        "16 75 -19 0",
        "4 -63 46 0",
        "91 -51 48 0",
        "48 5 46 0",
        "-31 7 70 0",
        "-35 24 93 0",
        "-48 -46 71 0",
        "-39 84 74 0",
        "-18 -61 -48 0",
        "-89 -99 -66 0",
        "-76 93 98 0",
        "76 -94 -86 0",
        "69 26 -62 0",
        "87 -30 35 0",
        "-93 81 -100 0",
        "33 -14 -51 0",
        "-70 58 -89 0",
        "-33 72 2 0",
        "69 1 43 0",
        "6 -28 -16 0",
        "64 18 68 0",
        "-10 -18 7 0",
        "-75 -5 -82 0",
        "89 -68 97 0",
        "-66 -77 59 0",
        "35 -45 63 0",
        "-51 -68 -28 0",
        "67 40 4 0",
        "45 5 71 0",
        "72 -1 -19 0",
        "76 16 45 0",
        "-49 69 70 0",
        "-75 93 -76 0",
        "-82 86 96 0",
        "95 98 32 0",
        "-13 5 29 0",
        "60 -91 14 0",
        "65 42 -33 0",
        "32 -2 -59 0",
        "-57 -48 82 0",
        "1 73 -15 0",
        "-76 89 48 0",
        "-29 83 -55 0",
        "-35 -54 76 0",
        "77 37 -59 0",
        "-26 10 83 0",
        "30 28 87 0",
        "48 28 56 0",
        "43 -3 6 0",
        "-12 20 11 0",
        "38 69 -57 0",
        "61 -54 -23 0",
        "48 20 96 0",
        "33 71 48 0",
        "-72 -17 -49 0",
        "-30 24 -62 0",
        "82 -4 39 0",
        "-73 99 -30 0",
        "-12 -33 29 0",
        "90 42 50 0",
        "-31 53 60 0",
        "78 20 -7 0",
        "-26 -97 -56 0",
        "14 76 -89 0",
        "76 -24 100 0",
        "61 -93 75 0",
        "24 -62 -33 0",
        "88 -3 52 0",
        "-100 72 91 0",
        "47 89 49 0",
        "69 -75 -2 0",
        "-57 18 47 0",
        "-45 -35 -26 0",
        "11 -78 -55 0",
        "-26 -13 -57 0",
        "-84 -28 45 0",
        "71 28 57 0",
        "-41 -60 39 0",
        "50 -67 -94 0",
        "73 -94 72 0",
        "99 51 58 0",
        "81 7 -57 0",
        "%",
        "0"
    ]
