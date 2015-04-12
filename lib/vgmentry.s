	; where the player is located in RAM.
	.alias music_player $e00
	.alias music_player_size 1024

	.alias music_initialize music_player
	.alias music_poll music_player+3
	.alias music_deinitialize music_player+6
	.alias copy_effect_from_shadow music_player+9
	.alias music_start_eventv music_player+12
	.alias music_stop_eventv music_player+15

	.alias music_playpos $60
