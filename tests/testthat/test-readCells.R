context("readCells")

rlogo_raster <- raster(system.file("external/rlogo.grd", package="raster"), band = 1)
rlogo_brick  <- brick(system.file("external/rlogo.grd", package="raster"))
rlogo_stack  <- stack(system.file("external/rlogo.grd", package="raster"))

rlogo_raster_bil <- raster("files/test_bil_1.bil")
rlogo_stack_bil <- stack(list(red = "files/test_bil_1.bil", 
                              green = "files/test_bil_2.bil",
                              blue = "files/test_bil_3.bil"))
rlogo_brick_bil <- brick(list(red = "files/test_bil_1.bil", 
                              green = "files/test_bil_2.bil",
                              blue = "files/test_bil_3.bil"))

rlogo_raster_tif <- raster("files/test_tif_1.tif")
rlogo_stack_tif <- stack("files/test_tif.tif")
names(rlogo_stack_tif) <- c("red", "green", "blue")
rlogo_brick_tif <- brick("files/test_tif.tif")
names(rlogo_brick_tif) <- c("red", "green", "blue")

set.seed(1022016)
random_pt <- sample(1:(nrow(rlogo_raster)*ncol(rlogo_raster)), 1)
row_col <- rowColFromCell(rlogo_raster, random_pt)
cells_rc <- cellFromRowCol(rlogo_raster, 
               rep(row_col[1, 1]:(row_col[1, 1] + 3), each = 3), 
               rep(row_col[1, 2]:(row_col[1, 2] + 2), times = 4))

test_that(".readCellsGDAL, getValuesBlock, `[`, and extract match for .grd, .bil, .tif, and RasterLayer", {
  gv_block_ras_grd <- getValuesBlock(rlogo_raster, row = row_col[1, 1], nrows = 4,
                                     col = row_col[1, 2], ncols = 3)
  sq_bracket_ras_grd <- rlogo_raster[cells_rc]
  extr_ras_grd <- raster::extract(rlogo_raster, cells_rc)
  
  gv_block_ras_bil <- getValuesBlock(rlogo_raster_bil, row = row_col[1, 1], nrows = 4,
                                     col = row_col[1, 2], ncols = 3)
  rcGDAL_ras_bil <- raster:::.readCellsGDAL(rlogo_raster_bil, cells = cells_rc, layers = 1)
  sq_bracket_ras_bil <- rlogo_raster_bil[cells_rc]
  extr_ras_bil <- raster::extract(rlogo_raster_bil, cells_rc)
  
  gv_block_ras_tif <- getValuesBlock(rlogo_raster_tif, row = row_col[1, 1], nrows = 4,
                                     col = row_col[1, 2], ncols = 3)
  rcGDAL_ras_tif <- raster:::.readCellsGDAL(rlogo_raster_tif, cells = cells_rc, layers = 1)
  sq_bracket_ras_tif <- rlogo_raster_tif[cells_rc]
  extr_ras_tif <- raster::extract(rlogo_raster_tif, cells_rc)
  
  expect_identical(gv_block_ras_grd, sq_bracket_ras_grd)
  expect_identical(gv_block_ras_grd, extr_ras_grd)
  expect_identical(gv_block_ras_grd, gv_block_ras_bil)
  expect_identical(gv_block_ras_grd, rcGDAL_ras_bil)
  expect_identical(gv_block_ras_grd, sq_bracket_ras_bil)
  expect_identical(gv_block_ras_grd, extr_ras_bil)
  expect_identical(gv_block_ras_grd, gv_block_ras_tif)
  expect_identical(gv_block_ras_grd, rcGDAL_ras_tif)
  expect_identical(gv_block_ras_grd, sq_bracket_ras_tif)
  expect_identical(gv_block_ras_grd, extr_ras_tif)
})

test_that("getValuesBlock, `[`, and extract match for .grd, .bil, .tif, and RasterStack", {
  gv_block_stack_grd <- getValuesBlock(rlogo_stack, row = row_col[1, 1], nrows = 4,
                                     col = row_col[1, 2], ncols = 3)
  sq_bracket_stack_grd <- rlogo_stack[cells_rc]
  extr_stack_grd <- raster::extract(rlogo_stack, cells_rc)
  
  gv_block_stack_bil <- getValuesBlock(rlogo_stack_bil, row = row_col[1, 1], nrows = 4,
                                     col = row_col[1, 2], ncols = 3)
  
  sq_bracket_stack_bil <- rlogo_stack_bil[cells_rc]
  extr_stack_bil <- raster::extract(rlogo_stack_bil, cells_rc)
  
  gv_block_stack_tif <- getValuesBlock(rlogo_stack_tif, row = row_col[1, 1], nrows = 4,
                                     col = row_col[1, 2], ncols = 3)
  
  sq_bracket_stack_tif <- rlogo_stack_tif[cells_rc]
  extr_stack_tif <- raster::extract(rlogo_stack_tif, cells_rc)
  
  expect_identical(gv_block_stack_grd, sq_bracket_stack_grd)
  expect_identical(gv_block_stack_grd, extr_stack_grd)
  expect_identical(gv_block_stack_grd, gv_block_stack_bil)
  expect_identical(gv_block_stack_grd, sq_bracket_stack_bil)
  expect_identical(gv_block_stack_grd, extr_stack_bil)
  expect_identical(gv_block_stack_grd, gv_block_stack_tif)
  expect_identical(gv_block_stack_grd, sq_bracket_stack_tif)
  expect_identical(gv_block_stack_grd, extr_stack_tif)
})

test_that(".readCellsGDAL, getValuesBlock, `[`, and extract match for .grd, .bil, .tif, and RasterBrick", {
  gv_block_brick_grd <- getValuesBlock(rlogo_brick, row = row_col[1, 1], nrows = 4,
                                     col = row_col[1, 2], ncols = 3)
  sq_bracket_brick_grd <- rlogo_brick[cells_rc]
  extr_brick_grd <- raster::extract(rlogo_brick, cells_rc)
  
  gv_block_brick_bil <- getValuesBlock(rlogo_brick_bil, row = row_col[1, 1], nrows = 4,
                                     col = row_col[1, 2], ncols = 3)
  
  sq_bracket_brick_bil <- rlogo_brick_bil[cells_rc]
  extr_brick_bil <- raster::extract(rlogo_brick_bil, cells_rc)
  
  gv_block_brick_tif <- getValuesBlock(rlogo_brick_tif, row = row_col[1, 1], nrows = 4,
                                     col = row_col[1, 2], ncols = 3)
  rcGDAL_brick_tif <- raster:::.readCellsGDAL(rlogo_brick_tif, cells = cells_rc, layers = 1:3)
  sq_bracket_brick_tif <- rlogo_brick_tif[cells_rc]
  extr_brick_tif <- raster::extract(rlogo_brick_tif, cells_rc)
  
  expect_identical(gv_block_brick_grd, sq_bracket_brick_grd)
  expect_identical(gv_block_brick_grd, extr_brick_grd)
  expect_identical(gv_block_brick_grd, gv_block_brick_bil)
  expect_identical(gv_block_brick_grd, sq_bracket_brick_bil)
  expect_identical(gv_block_brick_grd, extr_brick_bil)
  expect_identical(gv_block_brick_grd, gv_block_brick_tif)
  expect_identical(gv_block_brick_grd, rcGDAL_brick_tif)
  expect_identical(gv_block_brick_grd, sq_bracket_brick_tif)
  expect_identical(gv_block_brick_grd, extr_brick_tif)
})
